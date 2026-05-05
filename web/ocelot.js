"use strict";

let wasm = null;
let emu = 0;
let running = false;
let rafId = null;
let canvas, ctx, imageData;
let BUTTONS = {};

let audioCtx = null;
let audioNode = null;
let gainNode = null;
let audioEnabled = true;
let audioBufferLevel = 0;
let audioBufferCapacity = 1;
let audioFrameCounter = 0;

let db = null;
let currentRomName = "";
let currentRomTitle = "";
let currentSlot = 1;
let helpOpen = false;
let perfVisible = false;
let perfInterval = null;
let lastFrameTime = 0;
let frameInterval = 1000 / 59.7275;
let fpsFrames = 0;
let fpsWindowStart = 0;
const textDecoder = new TextDecoder();

const DEFAULT_KEY_MAP = Object.freeze({
    ArrowUp: "Up",
    ArrowDown: "Down",
    ArrowLeft: "Left",
    ArrowRight: "Right",
    KeyZ: "A",
    KeyX: "B",
    Enter: "Start",
    ShiftRight: "Select",
});

async function init() {
    canvas = document.getElementById("screen");
    ctx = canvas.getContext("2d");
    ctx.imageSmoothingEnabled = false;

    const ERRNO_NOSYS = 52;
    const wasiStubs = {
        fd_write: () => 0, fd_read: () => 0, fd_close: () => 0, fd_seek: () => 0,
        fd_tell: () => 0, fd_sync: () => 0, fd_datasync: () => 0,
        fd_advise: () => 0, fd_allocate: () => 0, fd_renumber: () => 0,
        fd_pread: () => ERRNO_NOSYS, fd_pwrite: () => ERRNO_NOSYS, fd_readdir: () => ERRNO_NOSYS,
        fd_fdstat_get: () => 0, fd_fdstat_set_flags: () => 0, fd_fdstat_set_rights: () => 0,
        fd_filestat_get: () => 0, fd_filestat_set_size: () => 0, fd_filestat_set_times: () => 0,
        fd_prestat_get: () => -1, fd_prestat_dir_name: () => -1,
        path_open: () => ERRNO_NOSYS, path_create_directory: () => ERRNO_NOSYS,
        path_link: () => ERRNO_NOSYS, path_readlink: () => ERRNO_NOSYS,
        path_rename: () => ERRNO_NOSYS, path_symlink: () => ERRNO_NOSYS,
        path_remove_directory: () => ERRNO_NOSYS, path_unlink_file: () => ERRNO_NOSYS,
        path_filestat_get: () => ERRNO_NOSYS, path_filestat_set_times: () => ERRNO_NOSYS,
        environ_get: () => 0,
        environ_sizes_get: (cp, sp) => {
            const view = new DataView(wasm.instance.exports.memory.buffer);
            view.setUint32(cp, 0, true);
            view.setUint32(sp, 0, true);
            return 0;
        },
        args_get: () => 0,
        args_sizes_get: (cp, sp) => {
            const view = new DataView(wasm.instance.exports.memory.buffer);
            view.setUint32(cp, 0, true);
            view.setUint32(sp, 0, true);
            return 0;
        },
        clock_time_get: () => 0,
        proc_exit: () => {},
        random_get: (buf, len) => {
            crypto.getRandomValues(new Uint8Array(wasm.instance.exports.memory.buffer, buf, len));
            return 0;
        },
    };

    try {
        const response = await fetch("ocelot.wasm");
        const bytes = await response.arrayBuffer();
        wasm = await WebAssembly.instantiate(bytes, {wasi_snapshot_preview1: wasiStubs});
    } catch (err) {
        showError("Failed to load ocelot.wasm");
        console.error(err);
        return;
    }

    const e = wasm.instance.exports;
    BUTTONS = {
        Up: e.ocelot_button_up(),
        Down: e.ocelot_button_down(),
        Left: e.ocelot_button_left(),
        Right: e.ocelot_button_right(),
        A: e.ocelot_button_a(),
        B: e.ocelot_button_b(),
        Start: e.ocelot_button_start(),
        Select: e.ocelot_button_select(),
    };

    canvas.addEventListener("click", togglePause);
    document.getElementById("rom-input").addEventListener("change", onFileSelected);
    document.getElementById("recent-roms").addEventListener("change", (ev) => {
        if (ev.target.value) loadRecentRom(ev.target.value);
        ev.target.selectedIndex = 0;
    });
    document.getElementById("audio-toggle").addEventListener("click", toggleAudio);
    document.getElementById("btn-quick-save").addEventListener("click", quickSave);
    document.getElementById("btn-quick-load").addEventListener("click", quickLoad);
    document.getElementById("btn-save").addEventListener("click", persistentSave);
    document.getElementById("btn-load").addEventListener("click", persistentLoad);
    document.getElementById("btn-fullscreen").addEventListener("click", toggleFullscreen);
    document.getElementById("help-btn").addEventListener("click", toggleHelp);
    document.getElementById("perf-toggle").addEventListener("click", togglePerf);
    document.getElementById("btn-pause").addEventListener("click", togglePause);
    document.getElementById("slot-select").addEventListener("change", (ev) => {
        currentSlot = parseInt(ev.target.value, 10);
    });
    document.getElementById("error-dismiss").addEventListener("click", hideError);

    document.addEventListener("keydown", onKeyDown);
    document.addEventListener("keyup", onKeyUp);

    const dropZone = document.getElementById("drop-zone");
    dropZone.addEventListener("dragover", (ev) => {
        ev.preventDefault();
        dropZone.classList.add("drag-over");
    });
    dropZone.addEventListener("dragleave", () => dropZone.classList.remove("drag-over"));
    dropZone.addEventListener("drop", (ev) => {
        ev.preventDefault();
        dropZone.classList.remove("drag-over");
        if (ev.dataTransfer.files.length > 0) loadRom(ev.dataTransfer.files[0]);
    });

    db = await openDB();
    await populateRecentRoms();

    setStatus(`Ready: ${readStaticString(e.ocelot_version_ptr, e.ocelot_version_len)}. Load a ROM to start playing.`);
}

function readMemory(ptr, len) {
    return new Uint8Array(wasm.instance.exports.memory.buffer, ptr, len);
}

function readStaticString(ptrFn, lenFn) {
    if (!wasm) return "";
    const e = wasm.instance.exports;
    const ptr = ptrFn();
    const len = lenFn();
    if (!ptr || !len) return "";
    return textDecoder.decode(readMemory(ptr, len));
}

function readSessionString(ptr, len) {
    if (!ptr || !len) return "";
    return textDecoder.decode(readMemory(ptr, len));
}

function getLastError() {
    if (!wasm) return "Unknown error";
    const e = wasm.instance.exports;
    const ptr = e.ocelot_last_error_ptr();
    const len = e.ocelot_last_error_len();
    return len ? textDecoder.decode(readMemory(ptr, len)) : "Unknown error";
}

function allocAndCopy(bytes) {
    const e = wasm.instance.exports;
    const ptr = e.ocelot_alloc(bytes.length);
    if (!ptr) return 0;
    readMemory(ptr, bytes.length).set(bytes);
    return ptr;
}

function freeBytes(ptr, len) {
    if (ptr) wasm.instance.exports.ocelot_free(ptr, len);
}

async function openDB() {
    return new Promise((resolve, reject) => {
        const req = indexedDB.open("ocelot-web", 1);
        req.onupgradeneeded = () => {
            const dbi = req.result;
            if (!dbi.objectStoreNames.contains("states")) dbi.createObjectStore("states");
            if (!dbi.objectStoreNames.contains("saves")) dbi.createObjectStore("saves");
            if (!dbi.objectStoreNames.contains("roms")) dbi.createObjectStore("roms");
        };
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
    });
}

function dbPut(storeName, key, value) {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, "readwrite");
        tx.objectStore(storeName).put(value, key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => reject(tx.error);
    });
}

function dbGet(storeName, key) {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, "readonly");
        const req = tx.objectStore(storeName).get(key);
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
    });
}

async function saveRecentRom(name, bytes) {
    await dbPut("roms", name, {name, bytes, timestamp: Date.now()});
}

async function getRecentRoms() {
    try {
        return await new Promise((resolve, reject) => {
            const tx = db.transaction("roms", "readonly");
            const req = tx.objectStore("roms").getAll();
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error);
        });
    } catch (_) {
        return [];
    }
}

async function populateRecentRoms() {
    const select = document.getElementById("recent-roms");
    while (select.options.length > 1) select.remove(1);
    const entries = await getRecentRoms();
    entries.sort((a, b) => b.timestamp - a.timestamp);
    if (entries.length === 0) {
        select.style.display = "none";
        return;
    }
    for (const entry of entries.slice(0, 10)) {
        const opt = document.createElement("option");
        opt.value = entry.name;
        opt.textContent = entry.name;
        select.appendChild(opt);
    }
    select.style.display = "";
}

async function loadRecentRom(name) {
    const entry = await dbGet("roms", name);
    if (!entry) return;
    const blob = new Blob([entry.bytes]);
    blob.name = entry.name;
    blob.arrayBuffer = () => Promise.resolve(entry.bytes.buffer.slice(
        entry.bytes.byteOffset, entry.bytes.byteOffset + entry.bytes.byteLength));
    await loadRom(blob);
}

async function initAudio() {
    if (audioCtx) return;
    try {
        audioCtx = new AudioContext({sampleRate: 48000});
        await audioCtx.audioWorklet.addModule("audio-worklet.js");
        const srcRate = wasm.instance.exports.ocelot_audio_sample_rate();
        audioNode = new AudioWorkletNode(audioCtx, "ocelot-audio", {
            numberOfOutputs: 1,
            channelCount: 2,
            channelCountMode: "explicit",
            channelInterpretation: "speakers",
            processorOptions: {srcRate},
        });
        audioNode.port.onmessage = (event) => {
            if (event.data && event.data.type === "level") {
                audioBufferLevel = event.data.count;
                audioBufferCapacity = event.data.capacity;
            }
        };
        gainNode = audioCtx.createGain();
        gainNode.gain.value = 1.0;
        audioNode.connect(gainNode);
        gainNode.connect(audioCtx.destination);
        if (!audioEnabled) audioCtx.suspend();
    } catch (err) {
        console.warn("Audio init failed", err);
        audioCtx = null;
        audioNode = null;
        gainNode = null;
    }
}

function toggleAudio() {
    audioEnabled = !audioEnabled;
    document.getElementById("audio-toggle").textContent = audioEnabled ? "ON" : "OFF";
    if (audioCtx) {
        if (audioEnabled) audioCtx.resume(); else audioCtx.suspend();
    }
}

function renderAudio() {
    if (!emu || !audioNode) return;
    const e = wasm.instance.exports;
    const sampleCount = e.ocelot_audio_buffer_len(emu);
    if (sampleCount === 0) return;
    const ptr = e.ocelot_audio_buffer_ptr(emu);
    const samples = new Int16Array(e.memory.buffer, ptr, sampleCount);
    audioNode.port.postMessage(samples.slice());
    e.ocelot_clear_audio_buffer(emu);
    if ((++audioFrameCounter % 6) === 0) {
        audioNode.port.postMessage("query-level");
    }
}

async function saveBatteryIfNeeded() {
    if (!emu || !db) return;
    const e = wasm.instance.exports;
    if (!e.ocelot_cartridge_has_battery(emu)) return;
    if (!e.ocelot_extract_save(emu)) return;
    const ptr = e.ocelot_save_buffer_ptr(emu);
    const len = e.ocelot_save_buffer_len(emu);
    const data = readMemory(ptr, len).slice();
    await dbPut("saves", currentRomName, data);
}

async function loadBatteryIfPresent() {
    if (!emu || !db) return;
    const e = wasm.instance.exports;
    if (!e.ocelot_cartridge_has_battery(emu)) return;
    const data = await dbGet("saves", currentRomName);
    if (!data) return;
    const ptr = allocAndCopy(data);
    if (!ptr) return;
    e.ocelot_load_save(emu, ptr, data.length);
    freeBytes(ptr, data.length);
}

async function destroyCurrentSession() {
    if (!emu) return;
    try {
        await saveBatteryIfNeeded();
    } catch (err) {
        console.warn("Battery save persistence failed", err);
    }
    running = false;
    if (rafId) cancelAnimationFrame(rafId);
    wasm.instance.exports.ocelot_destroy(emu);
    emu = 0;
}

function onFileSelected(ev) {
    if (ev.target.files.length > 0) loadRom(ev.target.files[0]);
}

async function loadRom(file) {
    if (!wasm) return;
    hideError();
    await destroyCurrentSession();
    await initAudio();

    const buffer = await file.arrayBuffer();
    const romBytes = new Uint8Array(buffer);
    const ptr = allocAndCopy(romBytes);
    if (!ptr) {
        showError("Failed to allocate ROM memory");
        return;
    }

    const e = wasm.instance.exports;
    emu = e.ocelot_create(ptr, romBytes.length);
    freeBytes(ptr, romBytes.length);
    if (!emu) {
        showError(getLastError());
        return;
    }

    currentRomName = file.name || "ROM";
    currentRomTitle = readSessionString(e.ocelot_rom_title_ptr(emu), e.ocelot_rom_title_len(emu)) || currentRomName;
    await saveRecentRom(currentRomName, romBytes);
    await populateRecentRoms();
    await loadBatteryIfPresent();

    if (audioCtx && audioEnabled && audioCtx.state === "suspended") {
        audioCtx.resume();
    }

    const mode = e.ocelot_is_cgb(emu) ? "CGB" : "DMG";
    setStatus(`Playing now: ${currentRomTitle} (${mode})`);
    frameInterval = 1000 / 59.7275;
    lastFrameTime = performance.now();
    fpsFrames = 0;
    fpsWindowStart = lastFrameTime;
    running = true;
    rafId = requestAnimationFrame(frameLoop);
}

function togglePause() {
    if (!emu) {
        showToast("Load a ROM first");
        return;
    }
    running = !running;
    document.getElementById("btn-pause").textContent = running ? "Pause" : "Resume";
    if (running) {
        lastFrameTime = performance.now();
        if (audioCtx && audioEnabled) audioCtx.resume();
        rafId = requestAnimationFrame(frameLoop);
        setStatus(`Playing now: ${currentRomTitle}`);
    } else {
        if (rafId) cancelAnimationFrame(rafId);
        if (audioCtx) audioCtx.suspend();
        setStatus("Paused");
    }
}

function frameLoop(now) {
    if (!running || !emu) return;
    rafId = requestAnimationFrame(frameLoop);
    tickEmulator(now);
}

function tickEmulator(now) {
    if (!running || !emu) return;
    if (now - lastFrameTime < frameInterval) return;
    lastFrameTime = now;

    const e = wasm.instance.exports;
    if (!e.ocelot_run_frame(emu)) {
        showError(getLastError());
        running = false;
        return;
    }

    renderAudio();
    const fbPtr = e.ocelot_framebuffer_ptr(emu);
    const fbLen = e.ocelot_framebuffer_len(emu);
    const fb = new Uint8ClampedArray(e.memory.buffer, fbPtr, fbLen);
    if (!imageData) imageData = ctx.createImageData(160, 144);
    const pixels = imageData.data;
    for (let src = 0, dst = 0; src < fb.length; src += 3, dst += 4) {
        pixels[dst] = fb[src];
        pixels[dst + 1] = fb[src + 1];
        pixels[dst + 2] = fb[src + 2];
        pixels[dst + 3] = 255;
    }
    ctx.putImageData(imageData, 0, 0);
    updateFps(now);
}

function updateFps(now) {
    fpsFrames++;
    const elapsed = now - fpsWindowStart;
    if (elapsed >= 500) {
        const fps = (fpsFrames * 1000) / elapsed;
        document.getElementById("fps-display").textContent = `${fps.toFixed(1)} FPS`;
        document.getElementById("perf-fps").textContent = `${fps.toFixed(1)} FPS`;
        document.getElementById("perf-frame-ms").textContent = `${(1000 / fps).toFixed(2)} ms`;
        fpsFrames = 0;
        fpsWindowStart = now;
    }
}

function quickSave() {
    persistentSave().catch((err) => {
        console.error(err);
        showToast("Save failed");
    });
}

function quickLoad() {
    persistentLoad().catch((err) => {
        console.error(err);
        showToast("Load failed");
    });
}

async function persistentSave() {
    if (!emu) {
        showToast("Load a ROM first");
        return;
    }
    const e = wasm.instance.exports;
    if (!e.ocelot_save_state(emu)) {
        showToast(getLastError());
        return;
    }
    const ptr = e.ocelot_save_state_ptr(emu);
    const len = e.ocelot_save_state_len(emu);
    const data = readMemory(ptr, len).slice();
    await dbPut("states", `${currentRomName}:slot${currentSlot}`, data);
    showToast(`Saved to slot ${currentSlot}`);
}

async function persistentLoad() {
    if (!emu) {
        showToast("Load a ROM first");
        return;
    }
    const data = await dbGet("states", `${currentRomName}:slot${currentSlot}`);
    if (!data) {
        showToast(`No save in slot ${currentSlot}`);
        return;
    }
    const ptr = allocAndCopy(data);
    if (!ptr) {
        showToast("Memory allocation failed");
        return;
    }
    const ok = wasm.instance.exports.ocelot_load_state(emu, ptr, data.length);
    freeBytes(ptr, data.length);
    showToast(ok ? `Loaded slot ${currentSlot}` : getLastError());
}

function toggleHelp() {
    const overlay = document.getElementById("help-overlay");
    helpOpen = !helpOpen;
    overlay.classList.toggle("visible", helpOpen);
}

function togglePerf() {
    perfVisible = !perfVisible;
    const hud = document.getElementById("perf-hud");
    hud.classList.toggle("hidden", !perfVisible);
    if (perfVisible) {
        updatePerf();
        perfInterval = setInterval(updatePerf, 500);
    } else if (perfInterval) {
        clearInterval(perfInterval);
        perfInterval = null;
    }
}

function updatePerf() {
    document.getElementById("perf-rom").textContent = currentRomTitle || "N/A";
    document.getElementById("perf-mode").textContent = emu ? (wasm.instance.exports.ocelot_is_cgb(emu) ? "CGB" : "DMG") : "N/A";
    document.getElementById("perf-audio").textContent =
        audioCtx && audioNode
            ? `${audioCtx.state} @ ${audioCtx.sampleRate} Hz (${audioBufferLevel}/${audioBufferCapacity})`
            : "OFF";
    document.getElementById("perf-wasm-mem").textContent =
        wasm ? `${(wasm.instance.exports.memory.buffer.byteLength / 1048576).toFixed(1)} MB` : "--";
}

function toggleFullscreen() {
    const container = document.getElementById("screen-container");
    if (document.fullscreenElement) {
        document.exitFullscreen();
        return;
    }
    container.requestFullscreen().catch(() => showToast("Fullscreen not available"));
}

function buttonForCode(code) {
    return DEFAULT_KEY_MAP[code] || null;
}

function onKeyDown(ev) {
    if (ev.code === "F1") {
        ev.preventDefault();
        toggleHelp();
        return;
    }
    if (ev.code === "F11") {
        ev.preventDefault();
        toggleFullscreen();
        return;
    }
    if (ev.code === "Space") {
        ev.preventDefault();
        togglePause();
        return;
    }
    if (ev.code === "F5") {
        ev.preventDefault();
        quickSave();
        return;
    }
    if (ev.code === "F8") {
        ev.preventDefault();
        quickLoad();
        return;
    }
    if (ev.code === "Escape" && helpOpen) {
        ev.preventDefault();
        toggleHelp();
        return;
    }
    if (!emu) return;
    const button = buttonForCode(ev.code);
    if (!button) return;
    wasm.instance.exports.ocelot_set_button(emu, BUTTONS[button], 1);
    ev.preventDefault();
}

function onKeyUp(ev) {
    if (!emu) return;
    const button = buttonForCode(ev.code);
    if (!button) return;
    wasm.instance.exports.ocelot_set_button(emu, BUTTONS[button], 0);
    ev.preventDefault();
}

function setStatus(msg) {
    document.getElementById("status").textContent = msg;
}

function showToast(msg) {
    const el = document.getElementById("toast");
    el.textContent = msg;
    el.classList.add("visible");
    clearTimeout(el._timer);
    el._timer = setTimeout(() => el.classList.remove("visible"), 2000);
}

function showError(msg) {
    document.getElementById("error-text").textContent = msg;
    document.getElementById("error-banner").classList.add("visible");
}

function hideError() {
    document.getElementById("error-banner").classList.remove("visible");
}

window.addEventListener("beforeunload", () => {
    if (emu) saveBatteryIfNeeded().catch(() => {});
});

init();
