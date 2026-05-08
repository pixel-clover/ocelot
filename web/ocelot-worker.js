"use strict";

let wasm = null;
let emu = 0;
let running = false;
const FRAME_INTERVAL = 1000 / 59.7275;
const FRAME_BYTES = 160 * 144 * 4;
let lastFrameTime = 0;
let tickTimer = null;
const bufferPool = [];
const audioBufferPool = [];
let audioFrameCounter = 0;
const textDecoder = new TextDecoder();
let lastTiming = {
    runMs: 0,
    frameCopyMs: 0,
    audioCopyMs: 0,
    totalMs: 0,
    audioSamples: 0,
};

// ─── WASI bridge ──────────────────────────────────────────────────────────────

async function createWasiBridge() {
    let memory = null;
    const errnoSuccess = 0;
    const errnoBadf = 8;
    const errnoNoent = 44;
    const errnoNosys = 52;
    const filetypeCharacterDevice = 2;
    const td = new TextDecoder();

    function view() { return new DataView(memory.buffer); }
    function bytes(ptr, len) { return new Uint8Array(memory.buffer, ptr, len); }
    function writeU32(ptr, value) { view().setUint32(ptr, value >>> 0, true); }
    function writeU64(ptr, value) { view().setBigUint64(ptr, BigInt(value), true); }
    function readU32(ptr) { return view().getUint32(ptr, true); }

    function fdWrite(fd, iovs, iovsLen, nwritten) {
        if (fd !== 1 && fd !== 2) return errnoBadf;
        let written = 0;
        const chunks = [];
        for (let i = 0; i < iovsLen; i++) {
            const base = readU32(iovs + i * 8);
            const len = readU32(iovs + i * 8 + 4);
            written += len;
            chunks.push(td.decode(bytes(base, len)));
        }
        const message = chunks.join("").replace(/\n$/, "");
        if (message.length > 0) {
            if (fd === 1) console.log(`[wasi stdout] ${message}`);
            else console.warn(`[wasi stderr] ${message}`);
        }
        writeU32(nwritten, written);
        return errnoSuccess;
    }

    const wasiImport = {
        args_sizes_get(argc, argvBufSize) { writeU32(argc, 0); writeU32(argvBufSize, 0); return errnoSuccess; },
        args_get() { return errnoSuccess; },
        environ_sizes_get(count, bufSize) { writeU32(count, 0); writeU32(bufSize, 0); return errnoSuccess; },
        environ_get() { return errnoSuccess; },
        clock_time_get(_clockId, _precision, timePtr) {
            writeU64(timePtr, BigInt(Math.floor(performance.timeOrigin * 1000000)) + BigInt(Math.floor(performance.now() * 1000000)));
            return errnoSuccess;
        },
        random_get(ptr, len) {
            const out = bytes(ptr, len);
            for (let offset = 0; offset < out.length; offset += 65536) {
                crypto.getRandomValues(out.subarray(offset, Math.min(offset + 65536, out.length)));
            }
            return errnoSuccess;
        },
        fd_write: fdWrite,
        fd_close(fd) { return fd <= 2 ? errnoSuccess : errnoBadf; },
        fd_fdstat_get(fd, statPtr) {
            if (fd > 2) return errnoBadf;
            bytes(statPtr, 24).fill(0);
            bytes(statPtr, 1)[0] = filetypeCharacterDevice;
            return errnoSuccess;
        },
        fd_fdstat_set_flags(fd) { return fd <= 2 ? errnoSuccess : errnoBadf; },
        fd_fdstat_set_rights(fd) { return fd <= 2 ? errnoSuccess : errnoBadf; },
        fd_advise() { return errnoBadf; },
        fd_allocate() { return errnoBadf; },
        fd_pread() { return errnoBadf; },
        fd_pwrite() { return errnoBadf; },
        fd_readdir() { return errnoBadf; },
        fd_renumber() { return errnoBadf; },
        fd_filestat_set_size() { return errnoBadf; },
        fd_filestat_set_times() { return errnoBadf; },
        fd_seek() { return errnoBadf; },
        fd_prestat_get() { return errnoBadf; },
        fd_prestat_dir_name() { return errnoBadf; },
        fd_read() { return errnoBadf; },
        fd_tell() { return errnoBadf; },
        fd_sync() { return errnoBadf; },
        fd_datasync() { return errnoBadf; },
        fd_filestat_get() { return errnoBadf; },
        path_open() { return errnoNoent; },
        path_filestat_get() { return errnoNoent; },
        path_filestat_set_times() { return errnoNoent; },
        path_create_directory() { return errnoNoent; },
        path_link() { return errnoNoent; },
        path_remove_directory() { return errnoNoent; },
        path_rename() { return errnoNoent; },
        path_readlink() { return errnoNoent; },
        path_symlink() { return errnoNoent; },
        path_unlink_file() { return errnoNoent; },
        clock_res_get(_clockId, resolutionPtr) { writeU64(resolutionPtr, 1); return errnoSuccess; },
        poll_oneoff(_inPtr, _outPtr, _nsubscriptions, neventsPtr) { writeU32(neventsPtr, 0); return errnoSuccess; },
        sched_yield() { return errnoSuccess; },
        sock_accept() { return errnoNosys; },
        sock_recv() { return errnoNosys; },
        sock_send() { return errnoNosys; },
        sock_shutdown() { return errnoNosys; },
        proc_exit(code) { throw new Error(`WASI proc_exit(${code})`); },
    };

    return {
        imports: {wasi_snapshot_preview1: wasiImport},
        initialize(instance) {
            memory = instance.exports.memory;
            if (typeof instance.exports._initialize === "function") {
                instance.exports._initialize();
            }
        },
    };
}

// ─── WASM init ────────────────────────────────────────────────────────────────

async function loadWasm() {
    const wasi = await createWasiBridge();
    const response = await fetch("ocelot.wasm");
    if (!response.ok) throw new Error(`Failed to fetch ocelot.wasm (${response.status})`);
    const wasmBytes = await response.arrayBuffer();
    wasm = await WebAssembly.instantiate(wasmBytes, wasi.imports);
    wasi.initialize(wasm.instance);
    const e = wasm.instance.exports;
    if (typeof e.hs_init !== "function") throw new Error("ocelot.wasm is missing hs_init");
    e.hs_init(0, 0);
}

// ─── Helpers ──────────────────────────────────────────────────────────────────

function readMemory(ptr, len) {
    return new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
}

function readString(ptr, len) {
    if (!ptr || !len) return "";
    return textDecoder.decode(readMemory(ptr, len));
}

function getLastError() {
    const e = wasm.instance.exports;
    return readString(e.ocelot_last_error_ptr(), e.ocelot_last_error_len()) || "Unknown error";
}

function wasmAlloc(bytes) {
    const e = wasm.instance.exports;
    const ptr = e.ocelot_alloc(bytes.length);
    if (!ptr) return 0;
    readMemory(ptr, bytes.length).set(bytes);
    return ptr;
}

function wasmFree(ptr, len) {
    if (ptr) wasm.instance.exports.ocelot_free(ptr, len);
}

// ─── Frame loop ───────────────────────────────────────────────────────────────

function getPoolBuffer() {
    return bufferPool.pop() || new ArrayBuffer(FRAME_BYTES);
}

function getAudioPoolBuffer(byteLength) {
    for (let i = audioBufferPool.length - 1; i >= 0; i--) {
        const buf = audioBufferPool[i];
        if (buf.byteLength >= byteLength) {
            audioBufferPool.splice(i, 1);
            return buf;
        }
    }
    return new ArrayBuffer(byteLength);
}

function runFrame() {
    const e = wasm.instance.exports;
    const frameStart = performance.now();
    if (!e.ocelot_run_frame(emu)) {
        postMessage({type: "frameError", message: getLastError()});
        running = false;
        return;
    }
    const afterRun = performance.now();

    const buf = getPoolBuffer();
    const fbPtr = e.ocelot_framebuffer_ptr(emu);
    const fbLen = e.ocelot_framebuffer_len(emu);
    new Uint8Array(buf).set(new Uint8Array(e.memory.buffer, fbPtr, fbLen));
    const afterFrameCopy = performance.now();
    postMessage({type: "frame", buffer: buf}, [buf]);

    const sampleCount = e.ocelot_audio_buffer_len(emu);
    let audioCopyMs = 0;
    if (sampleCount > 0) {
        const ptr = e.ocelot_audio_buffer_ptr(emu);
        const audioBytes = sampleCount * 2;
        const audioBuf = getAudioPoolBuffer(audioBytes);
        new Int16Array(audioBuf, 0, sampleCount).set(new Int16Array(e.memory.buffer, ptr, sampleCount));
        e.ocelot_clear_audio_buffer(emu);
        const afterAudioCopy = performance.now();
        audioCopyMs = afterAudioCopy - afterFrameCopy;
        const queryLevel = (++audioFrameCounter % 6) === 0;
        postMessage({type: "audio", buffer: audioBuf, samples: sampleCount, queryLevel}, [audioBuf]);
    }

    const frameEnd = performance.now();
    lastTiming = {
        runMs: afterRun - frameStart,
        frameCopyMs: afterFrameCopy - afterRun,
        audioCopyMs,
        totalMs: frameEnd - frameStart,
        audioSamples: sampleCount,
    };
}

function workerTick() {
    if (!running || !emu) {
        tickTimer = setTimeout(workerTick, 16);
        return;
    }
    const now = performance.now();
    const sinceLast = now - lastFrameTime;
    if (sinceLast < FRAME_INTERVAL - 1) {
        tickTimer = setTimeout(workerTick, Math.max(0, FRAME_INTERVAL - sinceLast - 1));
        return;
    }
    if (sinceLast > FRAME_INTERVAL * 4) {
        lastFrameTime = now - FRAME_INTERVAL;
    } else {
        lastFrameTime += FRAME_INTERVAL;
    }
    runFrame();
    tickTimer = setTimeout(workerTick, 0);
}

// ─── Message handler ──────────────────────────────────────────────────────────

self.onmessage = function (ev) {
    const msg = ev.data;
    const {type, id} = msg;
    try {
        switch (type) {
            case "loadRom": {
                running = false;
                if (emu) { wasm.instance.exports.ocelot_destroy(emu); emu = 0; }

                const romBytes = new Uint8Array(msg.romBuffer);
                const ptr = wasmAlloc(romBytes);
                if (!ptr) { postMessage({type: "romError", id, message: "Failed to allocate ROM memory"}); return; }
                const e = wasm.instance.exports;
                emu = e.ocelot_create(ptr, romBytes.length);
                wasmFree(ptr, romBytes.length);
                if (!emu) { postMessage({type: "romError", id, message: getLastError()}); return; }

                if (msg.batteryBuffer) {
                    const saveBytes = new Uint8Array(msg.batteryBuffer);
                    const savePtr = wasmAlloc(saveBytes);
                    if (savePtr) { e.ocelot_load_save(emu, savePtr, saveBytes.length); wasmFree(savePtr, saveBytes.length); }
                }

                const title = readString(e.ocelot_rom_title_ptr(emu), e.ocelot_rom_title_len(emu));
                const isCgb = !!e.ocelot_is_cgb(emu);
                const hasBattery = !!e.ocelot_cartridge_has_battery(emu);
                const wasmMemBytes = e.memory.buffer.byteLength;

                running = true;
                lastFrameTime = performance.now();
                if (tickTimer !== null) { clearTimeout(tickTimer); tickTimer = null; }
                tickTimer = setTimeout(workerTick, 0);

                postMessage({type: "romLoaded", id, title, isCgb, hasBattery, wasmMemBytes});
                break;
            }

            case "destroyRom": {
                running = false;
                if (tickTimer !== null) { clearTimeout(tickTimer); tickTimer = null; }
                if (emu) { wasm.instance.exports.ocelot_destroy(emu); emu = 0; }
                postMessage({type: "destroyRomOk", id});
                break;
            }

            case "setButton":
                if (emu) wasm.instance.exports.ocelot_set_button(emu, msg.button, msg.down ? 1 : 0);
                break;

            case "pause":
                running = false;
                break;

            case "resume":
                if (emu) {
                    running = true;
                    lastFrameTime = performance.now();
                    if (tickTimer === null) tickTimer = setTimeout(workerTick, 0);
                }
                break;

            case "saveState": {
                if (!emu) { postMessage({type: "saveStateError", id, message: "No ROM loaded"}); return; }
                const e = wasm.instance.exports;
                if (!e.ocelot_save_state(emu)) { postMessage({type: "saveStateError", id, message: getLastError()}); return; }
                const ptr = e.ocelot_save_state_ptr(emu);
                const len = e.ocelot_save_state_len(emu);
                const buf = new ArrayBuffer(len);
                new Uint8Array(buf).set(readMemory(ptr, len));
                postMessage({type: "saveStateData", id, buffer: buf}, [buf]);
                break;
            }

            case "loadState": {
                if (!emu) { postMessage({type: "loadStateError", id, message: "No ROM loaded"}); return; }
                const stateBytes = new Uint8Array(msg.buffer);
                const ptr = wasmAlloc(stateBytes);
                if (!ptr) { postMessage({type: "loadStateError", id, message: "Allocation failed"}); return; }
                const ok = wasm.instance.exports.ocelot_load_state(emu, ptr, stateBytes.length);
                wasmFree(ptr, stateBytes.length);
                postMessage(ok ? {type: "loadStateOk", id} : {type: "loadStateError", id, message: getLastError()});
                break;
            }

            case "extractSave": {
                if (!emu) { postMessage({type: "saveData", id, buffer: null, hasBattery: false}); return; }
                const e = wasm.instance.exports;
                if (!e.ocelot_cartridge_has_battery(emu)) { postMessage({type: "saveData", id, buffer: null, hasBattery: false}); return; }
                if (!e.ocelot_extract_save(emu)) { postMessage({type: "saveError", id, message: getLastError()}); return; }
                const ptr = e.ocelot_save_buffer_ptr(emu);
                const len = e.ocelot_save_buffer_len(emu);
                const buf = new ArrayBuffer(len);
                new Uint8Array(buf).set(readMemory(ptr, len));
                postMessage({type: "saveData", id, buffer: buf, hasBattery: true}, [buf]);
                break;
            }

            case "loadSave": {
                if (!emu) { postMessage({type: "loadSaveOk", id}); return; }
                const saveBytes = new Uint8Array(msg.buffer);
                const ptr = wasmAlloc(saveBytes);
                if (!ptr) { postMessage({type: "loadSaveError", id, message: "Allocation failed"}); return; }
                const ok = wasm.instance.exports.ocelot_load_save(emu, ptr, saveBytes.length);
                wasmFree(ptr, saveBytes.length);
                postMessage(ok ? {type: "loadSaveOk", id} : {type: "loadSaveError", id, message: getLastError()});
                break;
            }

            case "returnBuffer":
                if (msg.buffer && msg.buffer.byteLength === FRAME_BYTES) bufferPool.push(msg.buffer);
                break;

            case "returnAudioBuffer":
                if (msg.buffer && audioBufferPool.length < 8) audioBufferPool.push(msg.buffer);
                break;

            case "queryStats":
                postMessage({
                    type: "stats",
                    id,
                    wasmMemBytes: wasm ? wasm.instance.exports.memory.buffer.byteLength : 0,
                    timing: lastTiming,
                });
                break;

            default:
                console.warn("[worker] Unknown message type:", type);
        }
    } catch (err) {
        console.error("[worker] Error handling message", type, err);
        if (id !== undefined) postMessage({type: "error", id, message: err instanceof Error ? err.message : String(err)});
    }
};

// ─── Startup ──────────────────────────────────────────────────────────────────

(async () => {
    try {
        await loadWasm();
        const e = wasm.instance.exports;
        postMessage({
            type: "ready",
            buttons: {
                Up: e.ocelot_button_up(),
                Down: e.ocelot_button_down(),
                Left: e.ocelot_button_left(),
                Right: e.ocelot_button_right(),
                A: e.ocelot_button_a(),
                B: e.ocelot_button_b(),
                Start: e.ocelot_button_start(),
                Select: e.ocelot_button_select(),
            },
            version: readString(e.ocelot_version_ptr(), e.ocelot_version_len()),
            snapshotVersion: e.ocelot_snapshot_version(),
            audioSampleRate: e.ocelot_audio_sample_rate(),
        });
        tickTimer = setTimeout(workerTick, 16);
    } catch (err) {
        postMessage({type: "initError", message: err instanceof Error ? err.message : String(err)});
    }
})();
