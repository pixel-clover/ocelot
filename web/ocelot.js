"use strict";

let wasm = null;
let emu = 0;
let running = false;
let rafId = null;
let canvas, ctx, imageData;
let BUTTONS = {};

// Audio state
let audioCtx = null;
let audioNode = null;
let gainNode = null;
let audioEnabled = true;
let masterVolume = 70;
let audioBufferLevel = 0;
let audioBufferCapacity = 0;
let audioFrameCounter = 0;

// Storage
let db = null;
let storageNoticeShown = false;
let batterySaveTimer = null;
let batterySavePromise = null;

// ROM state
let currentRomName = "";
let currentRomTitle = "";
let currentSlot = 1;

// Overlay state
let helpOpen = false;
let aboutOpen = false;
// Counts simultaneously-open overlays; pause/resume only fire on 0->1 and 1->0 transitions.
let overlayDepth = 0;
let wasRunningBeforeOverlay = false;
// Auto-pause on tab hide; separate from user-initiated pause so the two don't interfere.
let pausedByVisibility = false;

// Perf HUD
let perfVisible = false;
let perfInterval = null;

// Timing
let lastFrameTime = 0;
let frameInterval = 1000 / 59.7275;
let fpsFrames = 0;
let fpsWindowStart = 0;
let lastFrameMs = 0;

const textDecoder = new TextDecoder();
const BATTERY_SAVE_INTERVAL_MS = 15000;
const STORAGE_DISABLED_MESSAGE = "Browser storage unavailable. Save states, battery saves, and recent ROMs are disabled.";

// Key map (mutable for remapping)
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
let keyMap = {...DEFAULT_KEY_MAP};

// GB buttons available for remapping (in display order)
const REMAP_BUTTONS = ["Up", "Down", "Left", "Right", "A", "B", "Start", "Select"];

const AXIS_THRESHOLD = 0.5;

async function init() {
    canvas = document.getElementById("screen");
    ctx = canvas.getContext("2d");
    ctx.imageSmoothingEnabled = false;

    const wasi = await createWasiBridge();

    try {
        const response = await fetch("ocelot.wasm");
        if (!response.ok) {
            throw new Error(`Failed to fetch ocelot.wasm (${response.status})`);
        }
        const bytes = await response.arrayBuffer();
        wasm = await WebAssembly.instantiate(bytes, wasi.imports);
        wasi.initialize(wasm.instance);
        initializeRuntime();
    } catch (err) {
        showError(err instanceof Error ? err.message : "Failed to load ocelot.wasm");
        console.error(err);
        return;
    }

    const e = wasm.instance.exports;
    BUTTONS = {
        Up:     e.ocelot_button_up(),
        Down:   e.ocelot_button_down(),
        Left:   e.ocelot_button_left(),
        Right:  e.ocelot_button_right(),
        A:      e.ocelot_button_a(),
        B:      e.ocelot_button_b(),
        Start:  e.ocelot_button_start(),
        Select: e.ocelot_button_select(),
    };

    canvas.addEventListener("click", togglePause);
    document.getElementById("rom-input").addEventListener("change", onFileSelected);
    document.getElementById("recent-roms").addEventListener("change", (ev) => {
        if (ev.target.value) loadRecentRom(ev.target.value);
        ev.target.selectedIndex = 0;
    });
    document.getElementById("audio-toggle").addEventListener("click", toggleAudio);
    document.getElementById("master-volume").addEventListener("input", onMasterVolumeChange);
    document.getElementById("btn-quick-save").addEventListener("click", quickSave);
    document.getElementById("btn-quick-load").addEventListener("click", quickLoad);
    document.getElementById("btn-save").addEventListener("click", persistentSave);
    document.getElementById("btn-load").addEventListener("click", persistentLoad);
    document.getElementById("slot-select").addEventListener("change", (ev) => {
        currentSlot = parseInt(ev.target.value, 10);
        saveSettings();
    });
    document.getElementById("btn-fullscreen").addEventListener("click", toggleFullscreen);
    document.getElementById("theme-toggle").addEventListener("click", toggleTheme);
    document.getElementById("settings-toggle").addEventListener("click", toggleSettings);
    document.getElementById("perf-toggle").addEventListener("click", togglePerf);
    document.getElementById("btn-pause").addEventListener("click", togglePause);
    document.getElementById("help-btn").addEventListener("click", toggleHelp);
    document.getElementById("help-close").addEventListener("click", toggleHelp);
    document.getElementById("about-btn").addEventListener("click", toggleAbout);
    document.getElementById("about-close").addEventListener("click", toggleAbout);
    document.getElementById("remap-reset").addEventListener("click", resetKeyMap);
    document.getElementById("error-dismiss").addEventListener("click", hideError);

    // Close overlays by clicking the backdrop
    document.getElementById("help-overlay").addEventListener("click", (ev) => {
        if (ev.target === ev.currentTarget) toggleHelp();
    });
    document.getElementById("about-overlay").addEventListener("click", (ev) => {
        if (ev.target === ev.currentTarget) toggleAbout();
    });

    document.addEventListener("keydown", onKeyDown);
    document.addEventListener("keyup", onKeyUp);
    document.addEventListener("visibilitychange", () => {
        if (document.visibilityState === "hidden") {
            saveBatteryIfNeeded({silent: true}).catch((err) => {
                console.warn("Battery save on visibilitychange failed", err);
            });
            if (running) {
                pausedByVisibility = true;
                stopFrameLoop();
                if (audioCtx) audioCtx.suspend();
            }
        } else if (pausedByVisibility) {
            pausedByVisibility = false;
            if (emu) {
                running = true;
                lastFrameTime = performance.now();
                if (audioCtx && audioEnabled) audioCtx.resume();
                rafId = requestAnimationFrame(frameLoop);
            }
        }
    });
    window.addEventListener("pagehide", () => {
        saveBatteryIfNeeded({silent: true}).catch((err) => {
            console.warn("Battery save on pagehide failed", err);
        });
    });

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

    try {
        db = await openDB();
        await populateRecentRoms();
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
    }

    loadSettings();
    initRemapUI();

    setStatus(`Ready: ${readStaticString(e.ocelot_version_ptr, e.ocelot_version_len)}. Load a ROM to start playing.`);
}

// ─── WASM helpers ────────────────────────────────────────────────────────────

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

async function createWasiBridge() {
    const {WASI, File, OpenFile, ConsoleStdout} =
        await import("https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2");
    const wasi = new WASI(
        [],
        [],
        [
            new OpenFile(new File(new Uint8Array())),
            ConsoleStdout.lineBuffered((msg) => console.log(`[wasi stdout] ${msg}`)),
            ConsoleStdout.lineBuffered((msg) => console.warn(`[wasi stderr] ${msg}`)),
        ],
    );
    return {
        imports: {wasi_snapshot_preview1: wasi.wasiImport},
        initialize(instance) {
            wasi.initialize(instance);
        },
    };
}

function initializeRuntime() {
    const e = wasm.instance.exports;
    if (typeof e.hs_init !== "function") {
        throw new Error("ocelot.wasm is missing hs_init");
    }
    e.hs_init(0, 0);
}

// ─── Settings persistence ─────────────────────────────────────────────────────

function loadSettings() {
    try {
        const saved = JSON.parse(localStorage.getItem("ocelot-settings") || "{}");
        if (saved.audioEnabled !== undefined) audioEnabled = saved.audioEnabled;
        if (saved.masterVolume !== undefined) {
            masterVolume = saved.masterVolume;
            document.getElementById("master-volume").value = saved.masterVolume;
            document.getElementById("master-volume-label").textContent = saved.masterVolume + "%";
        }
        if (saved.slot !== undefined) {
            currentSlot = saved.slot;
            document.getElementById("slot-select").value = saved.slot;
        }
        if (saved.keyMap) keyMap = {...DEFAULT_KEY_MAP, ...saved.keyMap};
        if (saved.theme) applyTheme(saved.theme);
    } catch (_) {}
    document.getElementById("audio-toggle").textContent = audioEnabled ? "ON" : "OFF";
}

function saveSettings() {
    try {
        localStorage.setItem("ocelot-settings", JSON.stringify({
            audioEnabled,
            masterVolume,
            slot: currentSlot,
            keyMap,
            theme: document.documentElement.getAttribute("data-theme") || "light",
        }));
    } catch (_) {}
}

// ─── Theme ────────────────────────────────────────────────────────────────────

function toggleTheme() {
    const current = document.documentElement.getAttribute("data-theme") || "light";
    applyTheme(current === "dark" ? "light" : "dark");
    saveSettings();
}

function applyTheme(theme) {
    document.documentElement.setAttribute("data-theme", theme);
    document.getElementById("theme-toggle").textContent = theme === "dark" ? "Dark" : "Light";
}

// ─── Keyboard remapping ───────────────────────────────────────────────────────

function keyDisplayName(code) {
    if (!code) return "·";
    if (code.startsWith("Key")) return code.slice(3);
    if (code.startsWith("Arrow")) return code.slice(5);
    if (code.startsWith("Digit")) return code.slice(5);
    if (code === "ShiftRight") return "R-Shift";
    if (code === "ShiftLeft") return "L-Shift";
    if (code === "Enter") return "Enter";
    if (code === "Space") return "Space";
    return code.replace(/([a-z])([A-Z])/g, "$1 $2");
}

function keyForButton(btn) {
    for (const [code, mapped] of Object.entries(keyMap)) {
        if (mapped === btn) return code;
    }
    return null;
}

function initRemapUI() {
    const grid = document.getElementById("remap-grid");
    grid.innerHTML = "";
    for (const btn of REMAP_BUTTONS) {
        const row = document.createElement("div");
        row.className = "remap-row";
        const lbl = document.createElement("label");
        lbl.textContent = btn;
        const rbtn = document.createElement("button");
        rbtn.className = "remap-btn";
        rbtn.dataset.btn = btn;
        rbtn.textContent = keyDisplayName(keyForButton(btn));
        rbtn.title = "Click to rebind " + btn;
        rbtn.addEventListener("click", () => startListening(rbtn, btn));
        row.append(lbl, rbtn);
        grid.appendChild(row);
    }
}

let activeRemapCleanup = null;

function startListening(rbtn, btn) {
    if (activeRemapCleanup) activeRemapCleanup();
    rbtn.classList.add("listening");
    rbtn.textContent = "Press a key...";

    function onKey(ev) {
        ev.preventDefault();
        ev.stopPropagation();
        cleanup();
        if (ev.code === "Escape") {
            rbtn.textContent = keyDisplayName(keyForButton(btn));
            return;
        }
        const newCode = ev.code;
        const existingBtn = keyMap[newCode];
        const oldCode = keyForButton(btn);
        if (existingBtn && existingBtn !== btn) {
            delete keyMap[newCode];
            if (oldCode) keyMap[oldCode] = existingBtn;
        }
        if (oldCode) delete keyMap[oldCode];
        keyMap[newCode] = btn;
        saveSettings();
        refreshRemapLabels();
    }

    function cleanup() {
        document.removeEventListener("keydown", onKey, true);
        rbtn.classList.remove("listening");
        activeRemapCleanup = null;
    }

    activeRemapCleanup = cleanup;
    document.addEventListener("keydown", onKey, true);
}

function refreshRemapLabels() {
    for (const rbtn of document.querySelectorAll(".remap-btn")) {
        rbtn.textContent = keyDisplayName(keyForButton(rbtn.dataset.btn));
    }
}

function resetKeyMap() {
    keyMap = {...DEFAULT_KEY_MAP};
    saveSettings();
    refreshRemapLabels();
}

// ─── Overlay helpers ──────────────────────────────────────────────────────────

function pauseForOverlay() {
    if (overlayDepth === 0) wasRunningBeforeOverlay = running;
    overlayDepth++;
    if (running) {
        running = false;
        if (rafId) cancelAnimationFrame(rafId);
        if (audioCtx) audioCtx.suspend();
    }
}

function resumeAfterOverlay() {
    if (overlayDepth > 0) overlayDepth--;
    if (overlayDepth > 0) return;
    if (wasRunningBeforeOverlay && emu) {
        running = true;
        lastFrameTime = performance.now();
        rafId = requestAnimationFrame(frameLoop);
        if (audioCtx && audioEnabled) audioCtx.resume();
    }
}

// ─── Storage ──────────────────────────────────────────────────────────────────

function hideRecentRoms() {
    const select = document.getElementById("recent-roms");
    if (!select) return;
    while (select.options.length > 1) select.remove(1);
    select.style.display = "none";
}

function disableStorage(message, err) {
    if (err) console.warn(message, err); else console.warn(message);
    stopBatterySaveTimer();
    if (db) { try { db.close(); } catch (_) {} }
    db = null;
    hideRecentRoms();
    if (!storageNoticeShown) {
        showToast(message);
        storageNoticeShown = true;
    }
}

function stopFrameLoop() {
    running = false;
    if (rafId !== null) {
        cancelAnimationFrame(rafId);
        rafId = null;
    }
}

function stopBatterySaveTimer() {
    if (batterySaveTimer !== null) {
        clearInterval(batterySaveTimer);
        batterySaveTimer = null;
    }
}

function startBatterySaveTimer() {
    stopBatterySaveTimer();
    if (!emu || !db) return;
    if (!wasm.instance.exports.ocelot_cartridge_has_battery(emu)) return;
    batterySaveTimer = setInterval(() => {
        saveBatteryIfNeeded({silent: true}).catch((err) => {
            console.warn("Periodic battery save failed", err);
        });
    }, BATTERY_SAVE_INTERVAL_MS);
}

async function openDB() {
    if (!("indexedDB" in window)) throw new Error("IndexedDB is unavailable");
    return new Promise((resolve, reject) => {
        const req = indexedDB.open("ocelot-web", 1);
        req.onupgradeneeded = () => {
            const dbi = req.result;
            if (!dbi.objectStoreNames.contains("states")) dbi.createObjectStore("states");
            if (!dbi.objectStoreNames.contains("saves"))  dbi.createObjectStore("saves");
            if (!dbi.objectStoreNames.contains("roms"))   dbi.createObjectStore("roms");
        };
        req.onsuccess = () => resolve(req.result);
        req.onerror   = () => reject(req.error);
        req.onblocked = () => reject(new Error("IndexedDB open was blocked"));
    });
}

function dbPut(storeName, key, value) {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, "readwrite");
        tx.objectStore(storeName).put(value, key);
        tx.oncomplete = () => resolve();
        tx.onerror    = () => reject(tx.error);
    });
}

function dbGet(storeName, key) {
    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, "readonly");
        const req = tx.objectStore(storeName).get(key);
        req.onsuccess = () => resolve(req.result);
        req.onerror   = () => reject(req.error);
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
            req.onerror   = () => reject(req.error);
        });
    } catch (_) {
        return [];
    }
}

async function populateRecentRoms() {
    const select = document.getElementById("recent-roms");
    while (select.options.length > 1) select.remove(1);
    if (!db) { select.style.display = "none"; return; }
    const entries = await getRecentRoms();
    entries.sort((a, b) => b.timestamp - a.timestamp);
    if (entries.length === 0) { select.style.display = "none"; return; }
    for (const entry of entries.slice(0, 10)) {
        const opt = document.createElement("option");
        opt.value = entry.name;
        opt.textContent = entry.name;
        select.appendChild(opt);
    }
    select.style.display = "";
}

async function loadRecentRom(name) {
    if (!db) { showToast("Persistent storage unavailable"); return; }
    let entry;
    try {
        entry = await dbGet("roms", name);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
        return;
    }
    if (!entry) return;
    const blob = new Blob([entry.bytes]);
    blob.name = entry.name;
    blob.arrayBuffer = () => Promise.resolve(entry.bytes.buffer.slice(
        entry.bytes.byteOffset, entry.bytes.byteOffset + entry.bytes.byteLength));
    await loadRom(blob);
}

// ─── Audio ────────────────────────────────────────────────────────────────────

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
        gainNode.gain.value = masterVolume / 100;
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
    saveSettings();
}

function onMasterVolumeChange() {
    masterVolume = parseInt(document.getElementById("master-volume").value, 10);
    document.getElementById("master-volume-label").textContent = masterVolume + "%";
    if (gainNode) gainNode.gain.value = masterVolume / 100;
    saveSettings();
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

// ─── Battery save ─────────────────────────────────────────────────────────────

async function saveBatteryIfNeeded({sessionId = emu, romName = currentRomName, silent = true} = {}) {
    if (!sessionId || !db) return false;
    if (batterySavePromise) return batterySavePromise;
    const e = wasm.instance.exports;
    const trackedPromise = (async () => {
        if (!e.ocelot_cartridge_has_battery(sessionId)) return false;
        if (!e.ocelot_extract_save(sessionId)) {
            if (!silent) showToast(getLastError());
            return false;
        }
        const ptr = e.ocelot_save_buffer_ptr(sessionId);
        const len = e.ocelot_save_buffer_len(sessionId);
        const data = readMemory(ptr, len).slice();
        try {
            await dbPut("saves", romName, data);
            return true;
        } catch (err) {
            disableStorage(STORAGE_DISABLED_MESSAGE, err);
            return false;
        }
    })().finally(() => {
        if (batterySavePromise === trackedPromise) batterySavePromise = null;
    });
    batterySavePromise = trackedPromise;
    return trackedPromise;
}

async function loadBatteryIfPresent() {
    if (!emu || !db) return;
    const e = wasm.instance.exports;
    if (!e.ocelot_cartridge_has_battery(emu)) return;
    let data;
    try {
        data = await dbGet("saves", currentRomName);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
        return;
    }
    if (!data) return;
    const ptr = allocAndCopy(data);
    if (!ptr) return;
    const ok = e.ocelot_load_save(emu, ptr, data.length);
    freeBytes(ptr, data.length);
    if (!ok) showToast(getLastError());
}

async function destroyCurrentSession() {
    if (!emu) return;
    const sessionId = emu;
    const romName = currentRomName;
    stopBatterySaveTimer();
    stopFrameLoop();
    try {
        await saveBatteryIfNeeded({sessionId, romName, silent: true});
    } catch (err) {
        console.warn("Battery save persistence failed", err);
    }
    wasm.instance.exports.ocelot_destroy(sessionId);
    emu = 0;
    currentRomName = "";
    currentRomTitle = "";
}

// ─── ROM loading ──────────────────────────────────────────────────────────────

function onFileSelected(ev) {
    if (ev.target.files.length > 0) loadRom(ev.target.files[0]);
}

async function decompressIfNeeded(file) {
    if (!(file.name || "").toLowerCase().endsWith(".zip")) return file;
    const { unzip } = await import("https://esm.sh/fflate@0.8.2");
    const buffer = await file.arrayBuffer();
    return new Promise((resolve, reject) => {
        unzip(new Uint8Array(buffer), (err, files) => {
            if (err) { reject(new Error("ZIP extraction failed: " + err.message)); return; }
            const ROM_EXTS = [".gb", ".gbc", ".sgb"];
            const entry = Object.entries(files).find(([name]) =>
                ROM_EXTS.some(ext => name.toLowerCase().endsWith(ext)));
            if (!entry) { reject(new Error("No .gb or .gbc ROM found inside ZIP")); return; }
            const [romName, romBytes] = entry;
            resolve(new File([romBytes], romName, { type: "application/octet-stream" }));
        });
    });
}

async function loadRom(file) {
    if (!wasm) return;
    hideError();
    // Close any open overlays without restoring state
    if (helpOpen) {
        document.getElementById("help-overlay").classList.remove("visible");
        helpOpen = false;
        overlayDepth = Math.max(0, overlayDepth - 1);
    }
    if (aboutOpen) {
        document.getElementById("about-overlay").classList.remove("visible");
        aboutOpen = false;
        overlayDepth = Math.max(0, overlayDepth - 1);
    }
    await destroyCurrentSession();
    await initAudio();
    try {
        file = await decompressIfNeeded(file);
        const buffer = await file.arrayBuffer();
        const romBytes = new Uint8Array(buffer);
        const ptr = allocAndCopy(romBytes);
        if (!ptr) { showError("Failed to allocate ROM memory"); return; }

        const e = wasm.instance.exports;
        emu = e.ocelot_create(ptr, romBytes.length);
        freeBytes(ptr, romBytes.length);
        if (!emu) { showError(getLastError()); return; }

        currentRomName = file.name || "ROM";
        currentRomTitle = readSessionString(
            e.ocelot_rom_title_ptr(emu), e.ocelot_rom_title_len(emu)) || currentRomName;
        if (db) {
            try {
                await saveRecentRom(currentRomName, romBytes);
                await populateRecentRoms();
            } catch (err) {
                disableStorage(STORAGE_DISABLED_MESSAGE, err);
            }
        }
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
        startBatterySaveTimer();
        rafId = requestAnimationFrame(frameLoop);
    } catch (err) {
        showError(err instanceof Error ? err.message : "Failed to load ROM");
        console.error(err);
    }
}

// ─── Emulation loop ───────────────────────────────────────────────────────────

function togglePause() {
    if (!emu) { showToast("Load a ROM first"); return; }
    running = !running;
    document.getElementById("btn-pause").textContent = running ? "Pause" : "Resume";
    if (running) {
        lastFrameTime = performance.now();
        if (audioCtx && audioEnabled) audioCtx.resume();
        rafId = requestAnimationFrame(frameLoop);
        setStatus(`Playing now: ${currentRomTitle}`);
    } else {
        stopFrameLoop();
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
    // Accumulate time rather than snapping to now, so the deficit persists
    // across RAF ticks and we hit ~59.7275 Hz on a 60 Hz display.
    // Cap at 4 frames to avoid runaway catch-up after the tab is backgrounded.
    if (now - lastFrameTime > frameInterval * 4) {
        lastFrameTime = now - frameInterval;
    } else {
        lastFrameTime += frameInterval;
    }

    pollGamepads();

    const t0 = performance.now();
    const e = wasm.instance.exports;
    if (!e.ocelot_run_frame(emu)) {
        showError(getLastError());
        stopFrameLoop();
        return;
    }

    renderAudio();
    const fbPtr = e.ocelot_framebuffer_ptr(emu);
    const fbLen = e.ocelot_framebuffer_len(emu);
    const fb = new Uint8ClampedArray(e.memory.buffer, fbPtr, fbLen);
    if (!imageData) imageData = ctx.createImageData(160, 144);
    const pixels = imageData.data;
    for (let src = 0, dst = 0; src < fb.length; src += 3, dst += 4) {
        pixels[dst]     = fb[src];
        pixels[dst + 1] = fb[src + 1];
        pixels[dst + 2] = fb[src + 2];
        pixels[dst + 3] = 255;
    }
    ctx.putImageData(imageData, 0, 0);
    lastFrameMs = performance.now() - t0;
    updateFps(now);
}

function updateFps(now) {
    fpsFrames++;
    const elapsed = now - fpsWindowStart;
    if (elapsed >= 500) {
        const fps = (fpsFrames * 1000) / elapsed;
        document.getElementById("fps-display").textContent = `${fps.toFixed(1)} FPS`;
        document.getElementById("perf-fps").textContent = `${fps.toFixed(1)} FPS`;
        document.getElementById("perf-frame-ms").textContent = `${lastFrameMs.toFixed(2)} ms`;
        fpsFrames = 0;
        fpsWindowStart = now;
    }
}

// ─── Gamepad ──────────────────────────────────────────────────────────────────

function pollGamepads() {
    if (!emu) return;
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    const e = wasm.instance.exports;

    for (let gi = 0; gi < gamepads.length; gi++) {
        const gp = gamepads[gi];
        if (!gp || !gp.connected) continue;

        const btn = (i) => gp.buttons.length > i && gp.buttons[i].pressed;
        const lx = gp.axes.length >= 2 ? gp.axes[0] : 0;
        const ly = gp.axes.length >= 2 ? gp.axes[1] : 0;

        e.ocelot_set_button(emu, BUTTONS.A,      btn(0) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.B,      btn(1) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Select, btn(8) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Start,  btn(9) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Up,    (btn(12) || ly < -AXIS_THRESHOLD) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Down,  (btn(13) || ly >  AXIS_THRESHOLD) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Left,  (btn(14) || lx < -AXIS_THRESHOLD) ? 1 : 0);
        e.ocelot_set_button(emu, BUTTONS.Right, (btn(15) || lx >  AXIS_THRESHOLD) ? 1 : 0);

        // Only use the first connected gamepad
        break;
    }
}

// ─── Save / Load ──────────────────────────────────────────────────────────────

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
    if (!emu) { showToast("Load a ROM first"); return; }
    if (!db)  { showToast("Persistent storage unavailable"); return; }
    const e = wasm.instance.exports;
    if (!e.ocelot_save_state(emu)) { showToast(getLastError()); return; }
    const ptr = e.ocelot_save_state_ptr(emu);
    const len = e.ocelot_save_state_len(emu);
    const data = readMemory(ptr, len).slice();
    try {
        await dbPut("states", `${currentRomName}:slot${currentSlot}`, data);
        showToast(`Saved to slot ${currentSlot}`);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
    }
}

async function persistentLoad() {
    if (!emu) { showToast("Load a ROM first"); return; }
    if (!db)  { showToast("Persistent storage unavailable"); return; }
    let data;
    try {
        data = await dbGet("states", `${currentRomName}:slot${currentSlot}`);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
        return;
    }
    if (!data) { showToast(`No save in slot ${currentSlot}`); return; }
    const ptr = allocAndCopy(data);
    if (!ptr)  { showToast("Memory allocation failed"); return; }
    const ok = wasm.instance.exports.ocelot_load_state(emu, ptr, data.length);
    freeBytes(ptr, data.length);
    showToast(ok ? `Loaded slot ${currentSlot}` : getLastError());
}

// ─── UI actions ───────────────────────────────────────────────────────────────

function toggleHelp() {
    const overlay = document.getElementById("help-overlay");
    if (helpOpen) {
        overlay.classList.remove("visible");
        helpOpen = false;
        resumeAfterOverlay();
    } else {
        pauseForOverlay();
        overlay.classList.add("visible");
        helpOpen = true;
    }
}

function toggleAbout() {
    const overlay = document.getElementById("about-overlay");
    if (aboutOpen) {
        overlay.classList.remove("visible");
        aboutOpen = false;
        resumeAfterOverlay();
    } else {
        pauseForOverlay();
        updateAboutInfo();
        overlay.classList.add("visible");
        aboutOpen = true;
    }
}

function updateAboutInfo() {
    document.getElementById("about-version").textContent =
        wasm ? readStaticString(wasm.instance.exports.ocelot_version_ptr, wasm.instance.exports.ocelot_version_len) : "--";
    document.getElementById("about-rom").textContent = currentRomTitle || "--";
    document.getElementById("about-mode").textContent =
        emu ? (wasm.instance.exports.ocelot_is_cgb(emu) ? "CGB" : "DMG") : "--";
    document.getElementById("about-audio").textContent =
        audioCtx ? `${audioCtx.sampleRate} Hz` : "OFF";
    document.getElementById("about-wasm-size").textContent =
        wasm ? `${(wasm.instance.exports.memory.buffer.byteLength / 1048576).toFixed(1)} MB` : "--";
}

function toggleSettings() {
    document.getElementById("settings-panel").classList.toggle("hidden");
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
    document.getElementById("perf-mode").textContent =
        emu ? (wasm.instance.exports.ocelot_is_cgb(emu) ? "CGB" : "DMG") : "N/A";
    document.getElementById("perf-audio").textContent =
        audioCtx && audioNode
            ? `${audioCtx.state}` +
              (audioBufferCapacity > 0 ? ` ${audioBufferLevel}/${audioBufferCapacity}` : "")
            : "OFF";
    document.getElementById("perf-wasm-mem").textContent =
        wasm ? `${(wasm.instance.exports.memory.buffer.byteLength / 1048576).toFixed(1)} MB` : "--";

    const gpDescriptions = [];
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    for (const gp of gamepads) {
        if (!gp || !gp.connected) continue;
        gpDescriptions.push(`${(gp.id || "?").slice(0, 20)} (${gp.buttons.length}b)`);
    }
    document.getElementById("perf-gamepads").textContent =
        gpDescriptions.length ? gpDescriptions.join("; ") : "none";
}

function toggleFullscreen() {
    const container = document.getElementById("screen-container");
    if (document.fullscreenElement) {
        document.exitFullscreen();
        return;
    }
    container.requestFullscreen().catch(() => showToast("Fullscreen not available"));
}

// ─── Input ────────────────────────────────────────────────────────────────────

function buttonForCode(code) {
    return keyMap[code] || null;
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
    if (ev.code === "Escape") {
        if (helpOpen)  { ev.preventDefault(); toggleHelp();  return; }
        if (aboutOpen) { ev.preventDefault(); toggleAbout(); return; }
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

// ─── UI helpers ───────────────────────────────────────────────────────────────

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

init();
