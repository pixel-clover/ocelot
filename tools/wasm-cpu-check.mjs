// Run blargg CPU test ROMs on a built ocelot.wasm and read each ROM's own
// verdict, so a miscompiled wasm artifact cannot ship silently.
//
// The desktop test suite exercises the native build only. The wasm build is a
// different compiler backend, and a GHC wasm codegen bug once shipped a CPU
// whose INC (HL) lost the Z flag on the 0xFF wrap: every native test stayed
// green while every game on the web build drifted off its hardware timeline.
// This check runs the same blargg ROMs against the actual wasm artifact under
// Node's WebAssembly engine, which is the same execution model as the browser.
//
// The cpu_instrs ROMs declare no cartridge RAM, so their only verdict channel
// is the serial port: each prints its name and then "Passed" or "Failed".
// The text is read through ocelot_drain_serial.
//
// Usage: node tools/wasm-cpu-check.mjs <ocelot.wasm> <rom.gb> [rom.gb ...]

import {readFileSync} from "fs";
import {basename} from "path";

const [wasmPath, ...romPaths] = process.argv.slice(2);
if (!wasmPath || romPaths.length === 0) {
    console.error("usage: node tools/wasm-cpu-check.mjs <ocelot.wasm> <rom.gb> [rom.gb ...]");
    process.exit(2);
}

const MAX_FRAMES = 7200; // two emulated minutes; the slowest CPU ROM needs far less
const POLL_FRAMES = 60;

function makeWasi(getMemory) {
    const view = () => new DataView(getMemory().buffer);
    const bytes = (p, l) => new Uint8Array(getMemory().buffer, p, l);
    const wU32 = (p, v) => view().setUint32(p, v >>> 0, true);
    const wU64 = (p, v) => view().setBigUint64(p, BigInt(v), true);
    const rU32 = (p) => view().getUint32(p, true);
    const ok = 0, badf = 8, noent = 44, nosys = 52;
    return new Proxy({
        args_sizes_get: (a, b) => (wU32(a, 0), wU32(b, 0), ok),
        args_get: () => ok,
        environ_sizes_get: (a, b) => (wU32(a, 0), wU32(b, 0), ok),
        environ_get: () => ok,
        clock_time_get: (_c, _p, t) => (wU64(t, BigInt(Date.now()) * 1000000n), ok),
        clock_res_get: (_c, r) => (wU64(r, 1), ok),
        random_get: (p, l) => (crypto.getRandomValues(bytes(p, l)), ok),
        fd_write: (fd, iovs, n, nw) => {
            let w = 0;
            for (let i = 0; i < n; i++) w += rU32(iovs + i * 8 + 4);
            wU32(nw, w);
            return ok;
        },
        fd_close: (fd) => (fd <= 2 ? ok : badf),
        fd_fdstat_get: (fd, st) => {
            if (fd > 2) return badf;
            bytes(st, 24).fill(0);
            bytes(st, 1)[0] = 2;
            return ok;
        },
        fd_fdstat_set_flags: (fd) => (fd <= 2 ? ok : badf),
        fd_fdstat_set_rights: (fd) => (fd <= 2 ? ok : badf),
        poll_oneoff: (_i, _o, _n, ne) => (wU32(ne, 0), ok),
        sched_yield: () => ok,
        proc_exit: (c) => {
            throw new Error(`WASI proc_exit(${c})`);
        },
    }, {
        get: (t, name) =>
            t[name]
            || (String(name).startsWith("path_") ? () => noent
                : String(name).startsWith("sock_") ? () => nosys
                : () => badf),
    });
}

async function loadEmulator() {
    let memory = null;
    const wasi = makeWasi(() => memory);
    const {instance} = await WebAssembly.instantiate(readFileSync(wasmPath), {
        wasi_snapshot_preview1: wasi,
    });
    memory = instance.exports.memory;
    if (typeof instance.exports._initialize === "function") instance.exports._initialize();
    instance.exports.hs_init(0, 0);
    return instance.exports;
}

function bytesOf(e, p, l) {
    return new Uint8Array(e.memory.buffer, p, l);
}

const decoder = new TextDecoder();

function drainSerialText(e, emu) {
    if (!e.ocelot_drain_serial || !e.ocelot_drain_serial(emu)) return "";
    const len = e.ocelot_serial_len(emu);
    if (!len) return "";
    return decoder.decode(bytesOf(e, e.ocelot_serial_ptr(emu), len));
}

let failures = 0;
for (const romPath of romPaths) {
    // A fresh instance per ROM: a trapped RTS would poison later runs.
    const e = await loadEmulator();
    const rom = readFileSync(romPath);
    const rp = e.ocelot_alloc(rom.length);
    bytesOf(e, rp, rom.length).set(rom);
    const emu = e.ocelot_create(rp, rom.length);
    e.ocelot_free(rp, rom.length);
    if (!emu) {
        console.log(`FAIL ${basename(romPath)}: ocelot_create failed`);
        failures++;
        continue;
    }
    let text = "";
    let verdict = null;
    for (let f = 0; f < MAX_FRAMES; f++) {
        if (!e.ocelot_run_frame(emu)) {
            verdict = "run_frame error";
            break;
        }
        e.ocelot_clear_audio_buffer(emu);
        if (f % POLL_FRAMES === POLL_FRAMES - 1) {
            text += drainSerialText(e, emu);
            if (text.includes("Passed")) {
                verdict = "pass";
                break;
            }
            if (text.includes("Failed")) {
                verdict = "fail";
                break;
            }
        }
    }
    const oneLine = text.replace(/\s+/g, " ").trim();
    if (verdict === "pass") {
        console.log(`ok   ${basename(romPath)}`);
    } else if (verdict === null) {
        console.log(`FAIL ${basename(romPath)}: no verdict after ${MAX_FRAMES} frames; serial: "${oneLine}"`);
        failures++;
    } else {
        console.log(`FAIL ${basename(romPath)}: ${verdict}; serial: "${oneLine}"`);
        failures++;
    }
    e.ocelot_destroy(emu);
}
process.exit(failures === 0 ? 0 : 1);
