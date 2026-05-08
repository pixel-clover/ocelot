"use strict";

// ─── Worker state ─────────────────────────────────────────────────────────────

let worker = null;
let workerReady = false;
let cmdSeq = 0;
const pendingCmds = new Map();

// Cached values received from the Worker
let BUTTONS = {};
let cachedVersion = "";
let cachedSnapshotVersion = 0;
let cachedAudioSampleRate = 48000;
let cachedIsCgb = false;
let cachedWasmMemBytes = 0;
let hasBattery = false;

// ─── Render state ─────────────────────────────────────────────────────────────

let canvas, ctx;
let framePixels = null;       // Uint8ClampedArray backing frameImageData
let frameImageData = null;    // ImageData for putImageData
let latestFrameReady = false;

// ─── Emulation state ─────────────────────────────────────────────────────────

let running = false;
let rafId = null;

// ─── Audio state ─────────────────────────────────────────────────────────────

let audioCtx = null;
let audioNode = null;
let gainNode = null;
let audioEnabled = true;
let masterVolume = 70;
let audioBufferLevel = 0;
let audioBufferCapacity = 0;

// ─── Video ────────────────────────────────────────────────────────────────────

let integerScale = 4;
let scanlinesEnabled = false;

// ─── Storage ─────────────────────────────────────────────────────────────────

let db = null;
let storageNoticeShown = false;
let batterySaveTimer = null;
let batterySavePromise = null;

// ─── ROM state ───────────────────────────────────────────────────────────────

let currentRomName = "";
let currentRomTitle = "";
let currentRomKey = "";
let currentSlot = 1;

// ─── Overlay state ────────────────────────────────────────────────────────────

let helpOpen = false;
let aboutOpen = false;
let overlayDepth = 0;
let wasRunningBeforeOverlay = false;
let pausedByVisibility = false;

// ─── Perf HUD ─────────────────────────────────────────────────────────────────

let perfVisible = false;
let perfInterval = null;

// ─── Timing ───────────────────────────────────────────────────────────────────

let fpsFrames = 0;
let fpsWindowStart = 0;
let lastFrameMs = 0;
let lastRafTime = 0;
let lastWorkerTiming = null;
let lastCanvasMs = 0;

const BATTERY_SAVE_INTERVAL_MS = 15000;
const STORAGE_DISABLED_MESSAGE = "Browser storage unavailable. Save states, battery saves, and recent ROMs are disabled.";

// ─── Input ────────────────────────────────────────────────────────────────────

const DEFAULT_KEY_MAP = Object.freeze({
    ArrowUp: "Up",
    ArrowDown: "Down",
    ArrowLeft: "Left",
    ArrowRight: "Right",
    KeyS: "A",
    KeyA: "B",
    Enter: "Start",
    ShiftRight: "Select",
});
let keyMap = {...DEFAULT_KEY_MAP};

const REMAP_BUTTONS = ["Up", "Down", "Left", "Right", "A", "B", "Start", "Select"];
let keyButtonsDown = new Set();
let gamepadButtonsDown = new Set();

const DEFAULT_GP_MAP = Object.freeze({Up: 12, Down: 13, Left: 14, Right: 15, A: 0, B: 1, Start: 9, Select: 8});
let gpMap = {...DEFAULT_GP_MAP};
const AXIS_THRESHOLD = 0.5;

// ─── Worker command helpers ───────────────────────────────────────────────────

function workerCmd(msg, transfer = []) {
    return new Promise((resolve, reject) => {
        const id = ++cmdSeq;
        pendingCmds.set(id, {resolve, reject});
        worker.postMessage({...msg, id}, transfer);
    });
}

function resolveCmd(id, value) {
    const pending = pendingCmds.get(id);
    if (pending) { pendingCmds.delete(id); pending.resolve(value); }
}

function rejectCmd(id, message) {
    const pending = pendingCmds.get(id);
    if (pending) { pendingCmds.delete(id); pending.reject(new Error(message)); }
}

// ─── Init ─────────────────────────────────────────────────────────────────────

async function init() {
    canvas = document.getElementById("screen");
    ctx = canvas.getContext("2d");
    ctx.imageSmoothingEnabled = false;
    applyIntegerScale(4);

    framePixels = new Uint8ClampedArray(160 * 144 * 4);
    frameImageData = new ImageData(framePixels, 160, 144);

    worker = new Worker("ocelot-worker.js");
    worker.onmessage = onWorkerMessage;
    worker.onerror = (err) => {
        showError(`Worker error: ${err.message || "unknown"}`);
        console.error("Worker error", err);
    };

    // Wait for 'ready' before wiring up the rest
    await new Promise((resolve, reject) => {
        const originalHandler = worker.onmessage;
        worker.onmessage = (ev) => {
            if (ev.data.type === "ready") {
                worker.onmessage = originalHandler;
                resolve(ev.data);
            } else if (ev.data.type === "initError") {
                reject(new Error(ev.data.message));
            }
        };
    }).then((data) => {
        BUTTONS = data.buttons;
        cachedVersion = data.version;
        cachedSnapshotVersion = data.snapshotVersion;
        cachedAudioSampleRate = data.audioSampleRate;
        workerReady = true;
    }).catch((err) => {
        showError(err instanceof Error ? err.message : "Failed to load ocelot.wasm");
        console.error(err);
        throw err;
    });

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
    document.querySelectorAll(".scale-btn[data-scale]").forEach(btn => {
        btn.addEventListener("click", () => {
            applyIntegerScale(+btn.dataset.scale);
            saveSettings();
        });
    });
    window.addEventListener("resize", () => applyIntegerScale(integerScale));
    document.addEventListener("fullscreenchange", () => applyIntegerScale(integerScale));
    document.addEventListener("webkitfullscreenchange", () => applyIntegerScale(integerScale));
    document.getElementById("settings-toggle").addEventListener("click", toggleSettings);
    document.getElementById("perf-toggle").addEventListener("click", togglePerf);
    document.getElementById("btn-pause").addEventListener("click", togglePause);
    document.getElementById("help-btn").addEventListener("click", toggleHelp);
    document.getElementById("help-close").addEventListener("click", toggleHelp);
    document.getElementById("about-btn").addEventListener("click", toggleAbout);
    document.getElementById("about-close").addEventListener("click", toggleAbout);
    document.getElementById("remap-reset").addEventListener("click", resetKeyMap);
    document.getElementById("gp-remap-reset").addEventListener("click", resetGpMap);
    document.getElementById("scanlines-toggle").addEventListener("click", () => {
        applyScanlines(!scanlinesEnabled);
        saveSettings();
    });
    document.getElementById("error-dismiss").addEventListener("click", hideError);

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
                worker.postMessage({type: "pause"});
                if (audioCtx) audioCtx.suspend();
            }
        } else if (pausedByVisibility) {
            pausedByVisibility = false;
            if (currentRomName) {
                running = true;
                lastRafTime = performance.now();
                worker.postMessage({type: "resume"});
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
    initGpRemapUI();

    setStatus(`Ready: ${cachedVersion}. Load a ROM to start playing.`);
}

// ─── Worker message handler ───────────────────────────────────────────────────

function onWorkerMessage(ev) {
    const msg = ev.data;
    switch (msg.type) {
        case "frame": {
            const t0 = performance.now();
            framePixels.set(new Uint8ClampedArray(msg.buffer));
            lastFrameMs = performance.now() - t0;
            latestFrameReady = true;
            // Return the buffer to the Worker pool immediately
            worker.postMessage({type: "returnBuffer", buffer: msg.buffer}, [msg.buffer]);
            break;
        }

        case "audio": {
            if (audioNode) {
                audioNode.port.postMessage(new Int16Array(msg.buffer, 0, msg.samples), [msg.buffer]);
                if (msg.queryLevel) audioNode.port.postMessage("query-level");
            } else {
                worker.postMessage({type: "returnAudioBuffer", buffer: msg.buffer}, [msg.buffer]);
            }
            break;
        }

        case "frameError":
            showError(msg.message || "Emulation error");
            stopFrameLoop();
            break;

        case "stats":
            cachedWasmMemBytes = msg.wasmMemBytes;
            lastWorkerTiming = msg.timing || lastWorkerTiming;
            resolveCmd(msg.id, msg);
            break;

        case "romLoaded":
            cachedIsCgb = msg.isCgb;
            hasBattery = msg.hasBattery;
            cachedWasmMemBytes = msg.wasmMemBytes;
            resolveCmd(msg.id, msg);
            break;

        case "romError":
            rejectCmd(msg.id, msg.message);
            break;

        case "destroyRomOk":
            resolveCmd(msg.id, null);
            break;

        case "saveStateData":
            resolveCmd(msg.id, msg.buffer);
            break;

        case "saveStateError":
            rejectCmd(msg.id, msg.message);
            break;

        case "loadStateOk":
            resolveCmd(msg.id, null);
            break;

        case "loadStateError":
            rejectCmd(msg.id, msg.message);
            break;

        case "saveData":
            resolveCmd(msg.id, msg);
            break;

        case "saveError":
            rejectCmd(msg.id, msg.message);
            break;

        case "loadSaveOk":
            resolveCmd(msg.id, null);
            break;

        case "loadSaveError":
            rejectCmd(msg.id, msg.message);
            break;

        case "error":
            rejectCmd(msg.id, msg.message);
            break;

        // "ready" is handled once during init; ignore repeats
        case "ready":
            break;

        default:
            console.warn("[main] Unknown worker message:", msg.type);
    }
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
        if (saved.gpMap) gpMap = {...DEFAULT_GP_MAP, ...saved.gpMap};
        if (saved.theme) applyTheme(saved.theme);
        if (saved.integerScale !== undefined) applyIntegerScale(saved.integerScale);
        else applyIntegerScale(4);
        if (saved.scanlines !== undefined) applyScanlines(saved.scanlines);
    } catch (_) {
        applyIntegerScale(4);
    }
    document.getElementById("audio-toggle").textContent = audioEnabled ? "ON" : "OFF";
}

function saveSettings() {
    try {
        localStorage.setItem("ocelot-settings", JSON.stringify({
            audioEnabled,
            masterVolume,
            slot: currentSlot,
            keyMap,
            gpMap,
            theme: document.documentElement.getAttribute("data-theme") || "dark",
            integerScale,
            scanlines: scanlinesEnabled,
        }));
    } catch (_) {
    }
}

// ─── Theme ────────────────────────────────────────────────────────────────────

function toggleTheme() {
    const current = document.documentElement.getAttribute("data-theme") || "dark";
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
    let name;
    if (code.startsWith("Key")) name = code.slice(3);
    else if (code.startsWith("Arrow")) name = code.slice(5);
    else if (code.startsWith("Digit")) name = code.slice(5);
    else if (code === "ShiftRight") name = "R-Shift";
    else if (code === "ShiftLeft") name = "L-Shift";
    else if (code === "Enter") name = "Enter";
    else if (code === "Space") name = "Space";
    else name = code.replace(/([a-z])([A-Z])/g, "$1 $2");
    return `[${name}]`;
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
        if (ev.code === "Escape") { rbtn.textContent = keyDisplayName(keyForButton(btn)); return; }
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

// ─── Gamepad remapping ────────────────────────────────────────────────────────

function gpBtnDisplayName(btnName) {
    const idx = gpMap[btnName];
    return idx !== undefined ? `(Btn ${idx})` : "·";
}

function initGpRemapUI() {
    const grid = document.getElementById("gp-remap-grid");
    if (!grid) return;
    grid.innerHTML = "";
    for (const btn of REMAP_BUTTONS) {
        const row = document.createElement("div");
        row.className = "remap-row";
        const lbl = document.createElement("label");
        lbl.textContent = btn;
        const rbtn = document.createElement("button");
        rbtn.className = "remap-btn";
        rbtn.dataset.gpBtn = btn;
        rbtn.textContent = gpBtnDisplayName(btn);
        rbtn.title = "Click then press a gamepad button to bind " + btn;
        rbtn.addEventListener("click", () => startGpListening(rbtn, btn));
        row.append(lbl, rbtn);
        grid.appendChild(row);
    }
}

let activeGpRemapCleanup = null;

function startGpListening(rbtn, btnName) {
    if (activeGpRemapCleanup) activeGpRemapCleanup();
    if (activeRemapCleanup) activeRemapCleanup();
    rbtn.classList.add("listening");
    rbtn.textContent = "Press a button…";

    const interval = setInterval(() => {
        const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
        for (const gp of gamepads) {
            if (!gp || !gp.connected) continue;
            for (let i = 0; i < gp.buttons.length; i++) {
                if (gp.buttons[i].pressed) {
                    cleanup();
                    const prev = gpMap[btnName];
                    for (const name of Object.keys(gpMap)) {
                        if (gpMap[name] === i && name !== btnName) gpMap[name] = prev;
                    }
                    gpMap[btnName] = i;
                    saveSettings();
                    refreshGpRemapLabels();
                    return;
                }
            }
        }
    }, 50);

    function onKey(ev) {
        ev.preventDefault();
        ev.stopPropagation();
        if (ev.code === "Escape") cleanup();
    }

    document.addEventListener("keydown", onKey, true);

    function cleanup() {
        clearInterval(interval);
        document.removeEventListener("keydown", onKey, true);
        rbtn.classList.remove("listening");
        rbtn.textContent = gpBtnDisplayName(btnName);
        activeGpRemapCleanup = null;
    }

    activeGpRemapCleanup = cleanup;
}

function refreshGpRemapLabels() {
    for (const rbtn of document.querySelectorAll(".remap-btn[data-gp-btn]")) {
        rbtn.textContent = gpBtnDisplayName(rbtn.dataset.gpBtn);
    }
}

function resetGpMap() {
    gpMap = {...DEFAULT_GP_MAP};
    saveSettings();
    refreshGpRemapLabels();
}

// ─── Overlay helpers ──────────────────────────────────────────────────────────

function pauseForOverlay() {
    if (overlayDepth === 0) wasRunningBeforeOverlay = running;
    overlayDepth++;
    if (running) {
        running = false;
        if (rafId) { cancelAnimationFrame(rafId); rafId = null; }
        worker.postMessage({type: "pause"});
        if (audioCtx) audioCtx.suspend();
    }
}

function resumeAfterOverlay() {
    if (overlayDepth > 0) overlayDepth--;
    if (overlayDepth > 0) return;
    if (wasRunningBeforeOverlay && currentRomName) {
        running = true;
        lastRafTime = performance.now();
        worker.postMessage({type: "resume"});
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
    if (!storageNoticeShown) { showToast(message); storageNoticeShown = true; }
}

function stopFrameLoop() {
    running = false;
    if (rafId !== null) { cancelAnimationFrame(rafId); rafId = null; }
}

function stopBatterySaveTimer() {
    if (batterySaveTimer !== null) { clearInterval(batterySaveTimer); batterySaveTimer = null; }
}

function startBatterySaveTimer() {
    stopBatterySaveTimer();
    if (!currentRomName || !db || !hasBattery) return;
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
            if (!dbi.objectStoreNames.contains("saves")) dbi.createObjectStore("saves");
            if (!dbi.objectStoreNames.contains("roms")) dbi.createObjectStore("roms");
        };
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
        req.onblocked = () => reject(new Error("IndexedDB open was blocked"));
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

async function sha256Hex(bytes) {
    const webCrypto = globalThis.crypto;
    if (!webCrypto || !webCrypto.subtle) throw new Error("WebCrypto SHA-256 is unavailable");
    const digest = await webCrypto.subtle.digest("SHA-256", bytes);
    return Array.from(new Uint8Array(digest), b => b.toString(16).padStart(2, "0")).join("");
}

function romStorageKey(hash) { return `sha256:${hash}`; }

async function saveRecentRom(key, name, bytes) {
    await dbPut("roms", key, {key, name, bytes, timestamp: Date.now()});
}

async function getRecentRoms() {
    try {
        return await new Promise((resolve, reject) => {
            const tx = db.transaction("roms", "readonly");
            const req = tx.objectStore("roms").getAll();
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error);
        });
    } catch (_) { return []; }
}

async function populateRecentRoms() {
    const select = document.getElementById("recent-roms");
    while (select.options.length > 1) select.remove(1);
    if (!db) { select.style.display = "none"; return; }
    const allEntries = await getRecentRoms();
    allEntries.sort((a, b) => b.timestamp - a.timestamp);
    const seen = new Set();
    const entries = allEntries.filter(e => {
        if (seen.has(e.name)) return false;
        seen.add(e.name);
        return true;
    });
    if (entries.length === 0) { select.style.display = "none"; return; }
    for (const entry of entries.slice(0, 10)) {
        const opt = document.createElement("option");
        opt.value = entry.key || entry.name;
        opt.textContent = entry.name;
        select.appendChild(opt);
    }
    select.style.display = "";
}

async function loadRecentRom(key) {
    if (!db) { showToast("Persistent storage unavailable"); return; }
    let entry;
    try {
        entry = await dbGet("roms", key);
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
        audioNode = new AudioWorkletNode(audioCtx, "ocelot-audio", {
            numberOfOutputs: 1,
            channelCount: 2,
            channelCountMode: "explicit",
            channelInterpretation: "speakers",
            processorOptions: {srcRate: cachedAudioSampleRate},
        });
        audioNode.port.onmessage = (event) => {
            if (event.data && event.data.type === "level") {
                audioBufferLevel = event.data.count;
                audioBufferCapacity = event.data.capacity;
            } else if (event.data && event.data.type === "return-buffer") {
                worker.postMessage({type: "returnAudioBuffer", buffer: event.data.buffer}, [event.data.buffer]);
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

// ─── Battery save ─────────────────────────────────────────────────────────────

async function saveBatteryIfNeeded({romKey = currentRomKey, silent = true} = {}) {
    if (!currentRomName || !db || !romKey || !hasBattery) return false;
    if (batterySavePromise) return batterySavePromise;
    const trackedPromise = (async () => {
        let result;
        try {
            result = await workerCmd({type: "extractSave"});
        } catch (err) {
            if (!silent) showToast(err.message || "Save extraction failed");
            return false;
        }
        if (!result.hasBattery || !result.buffer) return false;
        try {
            await dbPut("saves", romKey, new Uint8Array(result.buffer));
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

// ─── Destroy session ──────────────────────────────────────────────────────────

async function destroyCurrentSession() {
    if (!currentRomName) return;
    const romKey = currentRomKey;
    stopBatterySaveTimer();
    stopFrameLoop();
    try {
        await saveBatteryIfNeeded({romKey, silent: true});
    } catch (err) {
        console.warn("Battery save persistence failed", err);
    }
    try {
        await workerCmd({type: "destroyRom"});
    } catch (err) {
        console.warn("destroyRom failed", err);
    }
    currentRomName = "";
    currentRomTitle = "";
    currentRomKey = "";
    hasBattery = false;
    cachedIsCgb = false;
    keyButtonsDown = new Set();
    gamepadButtonsDown = new Set();
    latestFrameReady = false;
    syncLedState();
}

// ─── ROM loading ──────────────────────────────────────────────────────────────

function onFileSelected(ev) {
    if (ev.target.files.length > 0) loadRom(ev.target.files[0]);
}

async function decompressIfNeeded(file) {
    if (!(file.name || "").toLowerCase().endsWith(".zip")) return file;
    const {unzipFirstRom} = await import("./zip.js");
    return unzipFirstRom(file);
}

async function loadRom(file) {
    if (!workerReady) return;
    hideError();
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
        const romHash = await sha256Hex(romBytes);
        const romKey = romStorageKey(romHash);
        // Snapshot bytes for IndexedDB before transferring the buffer to the Worker
        const romBytesForDb = romBytes.slice();

        // Load existing battery save to send alongside the ROM
        let batteryBuffer = null;
        if (db) {
            try {
                const savedData = await dbGet("saves", romKey);
                if (savedData instanceof Uint8Array) {
                    batteryBuffer = savedData.buffer.slice(savedData.byteOffset, savedData.byteOffset + savedData.byteLength);
                } else if (savedData instanceof ArrayBuffer) {
                    batteryBuffer = savedData;
                }
            } catch (err) {
                disableStorage(STORAGE_DISABLED_MESSAGE, err);
            }
        }

        // Transfer ROM (and battery save) to Worker; buffer is detached after this point
        const romTransfer = [buffer];
        if (batteryBuffer) romTransfer.push(batteryBuffer);
        let romInfo;
        try {
            romInfo = await workerCmd({type: "loadRom", romBuffer: buffer, batteryBuffer: batteryBuffer || null}, romTransfer);
        } catch (err) {
            showError(err.message || "Failed to load ROM");
            return;
        }

        currentRomName = file.name || "ROM";
        currentRomKey = romKey;
        currentRomTitle = romInfo.title || currentRomName;

        if (db) {
            try {
                await saveRecentRom(currentRomKey, currentRomName, romBytesForDb);
                await populateRecentRoms();
            } catch (err) {
                disableStorage(STORAGE_DISABLED_MESSAGE, err);
            }
        }

        if (audioCtx && audioEnabled && audioCtx.state === "suspended") audioCtx.resume();

        const mode = cachedIsCgb ? "CGB" : "DMG";
        setStatus(`Playing now: ${currentRomTitle} (${mode})`);
        lastRafTime = performance.now();
        fpsFrames = 0;
        fpsWindowStart = lastRafTime;
        running = true;
        document.getElementById("btn-pause").textContent = "Pause";
        syncLedState();
        startBatterySaveTimer();
        rafId = requestAnimationFrame(frameLoop);
    } catch (err) {
        showError(err instanceof Error ? err.message : "Failed to load ROM");
        console.error(err);
    }
}

// ─── Render loop ──────────────────────────────────────────────────────────────

function togglePause() {
    if (!currentRomName) { showToast("Load a ROM first"); return; }
    running = !running;
    document.getElementById("btn-pause").textContent = running ? "Pause" : "Resume";
    if (running) {
        lastRafTime = performance.now();
        worker.postMessage({type: "resume"});
        if (audioCtx && audioEnabled) audioCtx.resume();
        rafId = requestAnimationFrame(frameLoop);
        setStatus(`Playing now: ${currentRomTitle}`);
    } else {
        stopFrameLoop();
        worker.postMessage({type: "pause"});
        if (audioCtx) audioCtx.suspend();
        setStatus("Paused");
    }
    syncLedState();
}

function frameLoop(now) {
    if (!running) return;
    rafId = requestAnimationFrame(frameLoop);
    pollGamepads();
    if (latestFrameReady) {
        const t0 = performance.now();
        ctx.putImageData(frameImageData, 0, 0);
        lastCanvasMs = performance.now() - t0;
        latestFrameReady = false;
        updateFps(now);
    }
}

function updateFps(now) {
    fpsFrames++;
    const elapsed = now - fpsWindowStart;
    if (elapsed >= 500) {
        const fps = (fpsFrames * 1000) / elapsed;
        document.getElementById("fps-display").textContent = `${fps.toFixed(1)} FPS`;
        document.getElementById("perf-fps").textContent = `${fps.toFixed(1)} FPS`;
        updatePerfTiming();
        fpsFrames = 0;
        fpsWindowStart = now;
    }
}

// ─── Gamepad ──────────────────────────────────────────────────────────────────

function pollGamepads() {
    if (!currentRomName) return;
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    const nextDown = new Set();
    for (let gi = 0; gi < gamepads.length; gi++) {
        const gp = gamepads[gi];
        if (!gp || !gp.connected) continue;
        const btn = (i) => gp.buttons.length > i && gp.buttons[i].pressed;
        const lx = gp.axes.length >= 2 ? gp.axes[0] : 0;
        const ly = gp.axes.length >= 2 ? gp.axes[1] : 0;
        if (btn(gpMap.A)) nextDown.add("A");
        if (btn(gpMap.B)) nextDown.add("B");
        if (btn(gpMap.Select)) nextDown.add("Select");
        if (btn(gpMap.Start)) nextDown.add("Start");
        if (btn(gpMap.Up) || ly < -AXIS_THRESHOLD) nextDown.add("Up");
        if (btn(gpMap.Down) || ly > AXIS_THRESHOLD) nextDown.add("Down");
        if (btn(gpMap.Left) || lx < -AXIS_THRESHOLD) nextDown.add("Left");
        if (btn(gpMap.Right) || lx > AXIS_THRESHOLD) nextDown.add("Right");
        break;
    }
    gamepadButtonsDown = nextDown;
    syncAllButtons();
}

// ─── Save / Load ──────────────────────────────────────────────────────────────

function quickSave() {
    persistentSave().catch((err) => { console.error(err); showToast("Save failed"); });
}

function quickLoad() {
    persistentLoad().catch((err) => { console.error(err); showToast("Load failed"); });
}

async function persistentSave() {
    if (!currentRomName) { showToast("Load a ROM first"); return; }
    if (!db) { showToast("Persistent storage unavailable"); return; }
    if (!currentRomKey) { showToast("ROM identity unavailable"); return; }
    let stateBuffer;
    try {
        stateBuffer = await workerCmd({type: "saveState"});
    } catch (err) {
        showToast(err.message || "Save failed");
        return;
    }
    try {
        await dbPut("states", `${currentRomKey}:slot${currentSlot}`, new Uint8Array(stateBuffer));
        showToast(`Saved to slot ${currentSlot}`);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
    }
}

async function persistentLoad() {
    if (!currentRomName) { showToast("Load a ROM first"); return; }
    if (!db) { showToast("Persistent storage unavailable"); return; }
    if (!currentRomKey) { showToast("ROM identity unavailable"); return; }
    let data;
    try {
        data = await dbGet("states", `${currentRomKey}:slot${currentSlot}`);
    } catch (err) {
        disableStorage(STORAGE_DISABLED_MESSAGE, err);
        return;
    }
    if (!data) { showToast(`No save in slot ${currentSlot}`); return; }
    const buf = data instanceof Uint8Array
        ? data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength)
        : data;
    try {
        await workerCmd({type: "loadState", buffer: buf}, [buf]);
        showToast(`Loaded slot ${currentSlot}`);
    } catch (err) {
        showToast(err.message || "Load failed");
    }
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
    document.getElementById("about-version").textContent = cachedVersion || "--";
    document.getElementById("about-save-version").textContent = cachedSnapshotVersion ? `v${cachedSnapshotVersion}` : "--";
    document.getElementById("about-rom").textContent = currentRomTitle || "--";
    document.getElementById("about-mode").textContent = currentRomName ? (cachedIsCgb ? "CGB" : "DMG") : "--";
    document.getElementById("about-audio").textContent = audioCtx ? `${audioCtx.sampleRate} Hz` : "OFF";
    document.getElementById("about-wasm-size").textContent =
        cachedWasmMemBytes ? `${(cachedWasmMemBytes / 1048576).toFixed(1)} MB` : "--";
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
    document.getElementById("perf-mode").textContent = currentRomName ? (cachedIsCgb ? "CGB" : "DMG") : "N/A";
    if (!running) {
        document.getElementById("perf-fps").textContent = "-- FPS";
    }
    updatePerfTiming();
    document.getElementById("perf-audio").textContent =
        audioCtx && audioNode
            ? `${audioCtx.state}` + (audioBufferCapacity > 0 ? ` ${audioBufferLevel}/${audioBufferCapacity}` : "")
            : "OFF";
    document.getElementById("perf-wasm-mem").textContent =
        cachedWasmMemBytes ? `${(cachedWasmMemBytes / 1048576).toFixed(1)} MB` : "--";

    if (perfVisible && currentRomName) {
        workerCmd({type: "queryStats"}).then((msg) => {
            cachedWasmMemBytes = msg.wasmMemBytes;
            lastWorkerTiming = msg.timing || lastWorkerTiming;
            document.getElementById("perf-wasm-mem").textContent =
                `${(cachedWasmMemBytes / 1048576).toFixed(1)} MB`;
            updatePerfTiming();
        }).catch(() => {});
    }

    const gpDescriptions = [];
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    for (const gp of gamepads) {
        if (!gp || !gp.connected) continue;
        gpDescriptions.push(`${(gp.id || "?").slice(0, 20)} (${gp.buttons.length}b)`);
    }
    document.getElementById("perf-gamepads").textContent =
        gpDescriptions.length ? gpDescriptions.join("; ") : "none";
}

function formatMs(value) {
    return Number.isFinite(value) ? `${value.toFixed(2)} ms` : "--";
}

function updatePerfTiming() {
    document.getElementById("perf-frame-ms").textContent = formatMs(lastFrameMs);
    document.getElementById("perf-canvas-ms").textContent = formatMs(lastCanvasMs);
    if (!lastWorkerTiming) {
        document.getElementById("perf-worker-total-ms").textContent = "--";
        document.getElementById("perf-wasm-run-ms").textContent = "--";
        document.getElementById("perf-worker-copy-ms").textContent = "--";
        document.getElementById("perf-audio-copy-ms").textContent = "--";
        return;
    }
    document.getElementById("perf-worker-total-ms").textContent = formatMs(lastWorkerTiming.totalMs);
    document.getElementById("perf-wasm-run-ms").textContent = formatMs(lastWorkerTiming.runMs);
    document.getElementById("perf-worker-copy-ms").textContent = formatMs(lastWorkerTiming.frameCopyMs);
    document.getElementById("perf-audio-copy-ms").textContent =
        `${formatMs(lastWorkerTiming.audioCopyMs)} / ${lastWorkerTiming.audioSamples || 0}`;
}

// ─── GB hardware state ────────────────────────────────────────────────────────

function syncLedState() {
    const c = document.getElementById("screen-container");
    if (!c) return;
    c.classList.remove("emu-playing", "emu-paused");
    if (!currentRomName) return;
    c.classList.add(running ? "emu-playing" : "emu-paused");
}

function applyScanlines(enabled) {
    scanlinesEnabled = enabled;
    const c = document.getElementById("screen-container");
    if (c) c.classList.toggle("scanlines", enabled);
    const btn = document.getElementById("scanlines-toggle");
    if (btn) {
        btn.textContent = enabled ? "On" : "Off";
        btn.classList.toggle("active", enabled);
    }
}

// ─── Integer scaling / fit / stretch ─────────────────────────────────────────

function windowAvailW() {
    const container = document.getElementById("screen-container");
    if (!container) return window.innerWidth;
    const parent = container.parentElement;
    const style = getComputedStyle(parent);
    return parent.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);
}

function windowAvailH() {
    const container = document.getElementById("screen-container");
    if (!container) return window.innerHeight;
    const parent = container.parentElement;
    const style = getComputedStyle(parent);
    const paddingV = parseFloat(style.paddingTop) + parseFloat(style.paddingBottom);
    const gap = parseFloat(style.gap) || 0;
    let siblingsH = 0, visibleSiblingCount = 0;
    for (const child of parent.children) {
        if (child === container) continue;
        if (getComputedStyle(child).display === "none") continue;
        siblingsH += child.offsetHeight;
        visibleSiblingCount++;
    }
    const cs = getComputedStyle(container);
    const containerExtrasV = parseFloat(cs.paddingTop) + parseFloat(cs.paddingBottom)
        + parseFloat(cs.borderTopWidth) + parseFloat(cs.borderBottomWidth);
    return Math.max(144, window.innerHeight - siblingsH - paddingV - gap * visibleSiblingCount - containerExtrasV);
}

function computeBestScale() {
    return Math.max(1, Math.floor(Math.min(windowAvailW() / 160, windowAvailH() / 144)));
}

function applyIntegerScale(n) {
    integerScale = n;
    const isFs = !!document.fullscreenElement;
    const availW = isFs ? window.innerWidth : windowAvailW();
    const availH = isFs ? window.innerHeight : windowAvailH();
    let w, h;
    if (n === -1 || isFs) {
        if (isFs) {
            const s = Math.max(1, Math.floor(Math.min(availW / 160, availH / 144)));
            w = 160 * s; h = 144 * s;
        } else {
            const s = Math.min(availW / 160, availH / 144);
            w = Math.floor(160 * s); h = Math.floor(144 * s);
        }
    } else {
        const s = n > 0 ? n : computeBestScale();
        w = 160 * s; h = 144 * s;
    }
    if (canvas) {
        canvas.style.width = w + "px";
        canvas.style.height = h + "px";
        canvas.style.imageRendering = (n < 0 && !isFs) ? "auto" : "pixelated";
    }
    const overlay = document.getElementById("scanlines-overlay");
    if (overlay) {
        overlay.style.width = w + "px";
        overlay.style.height = h + "px";
        if (isFs) {
            overlay.style.left = Math.floor((window.innerWidth - w) / 2) + "px";
            overlay.style.top = Math.floor((window.innerHeight - h) / 2) + "px";
        } else {
            overlay.style.left = "0";
            overlay.style.top = "0";
        }
    }
    document.querySelectorAll(".scale-btn[data-scale]").forEach(btn => {
        btn.classList.toggle("active", +btn.dataset.scale === n);
    });
}

function toggleFullscreen() {
    const container = document.getElementById("screen-container");
    if (document.fullscreenElement) { document.exitFullscreen(); return; }
    container.requestFullscreen().catch(() => showToast("Fullscreen not available"));
}

// ─── Input ────────────────────────────────────────────────────────────────────

function buttonForCode(code) { return keyMap[code] || null; }

function syncButton(button) {
    if (!currentRomName || BUTTONS[button] === undefined) return;
    const down = keyButtonsDown.has(button) || gamepadButtonsDown.has(button);
    worker.postMessage({type: "setButton", button: BUTTONS[button], down});
}

function syncAllButtons() {
    for (const button of REMAP_BUTTONS) syncButton(button);
}

function onKeyDown(ev) {
    if (ev.code === "F1") { ev.preventDefault(); toggleHelp(); return; }
    if (ev.code === "F11") { ev.preventDefault(); toggleFullscreen(); return; }
    if (ev.code === "Space") { ev.preventDefault(); togglePause(); return; }
    if (ev.code === "F5") { ev.preventDefault(); quickSave(); return; }
    if (ev.code === "F8") { ev.preventDefault(); quickLoad(); return; }
    if (ev.code === "Escape") {
        if (helpOpen) { ev.preventDefault(); toggleHelp(); return; }
        if (aboutOpen) { ev.preventDefault(); toggleAbout(); return; }
    }
    if (!currentRomName) return;
    const button = buttonForCode(ev.code);
    if (!button) return;
    keyButtonsDown.add(button);
    syncButton(button);
    ev.preventDefault();
}

function onKeyUp(ev) {
    if (!currentRomName) return;
    const button = buttonForCode(ev.code);
    if (!button) return;
    keyButtonsDown.delete(button);
    syncButton(button);
    ev.preventDefault();
}

// ─── UI helpers ───────────────────────────────────────────────────────────────

function setStatus(msg) { document.getElementById("status").textContent = msg; }

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

function hideError() { document.getElementById("error-banner").classList.remove("visible"); }

init();
