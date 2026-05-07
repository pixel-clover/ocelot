## Project Roadmap

This document outlines the features implemented in the Ocelot emulator, and the future goals for the project.

> [!IMPORTANT]
> This roadmap is a work in progress and is subject to change.

### Core System

- [x] Project scaffolding (Stack, hpack, fourmolu, HLint, flake-based dev shell)
- [x] Public API facade module (`Ocelot`) with versioned entry point
- [x] SM83 register file with 8-bit registers, 16-bit pairs, and Z/N/H/C flag accessors
- [x] DMG post-boot register state constant
- [x] SM83 unprefixed opcode decoder (full set) with placeholder M-cycle timing
- [x] SM83 CB-prefix opcode decoder (full set: RLC/RRC/RL/RR/SLA/SRA/SWAP/SRL plus BIT/RES/SET on every register and (HL))
- [x] Bit/rotate/shift helpers (RLC/RRC/RL/RR/SLA/SRA/SWAP/SRL/BIT/RES/SET)
- [x] ALU helpers (add/adc/sub/sbc/and/or/xor/cp/inc/dec) with correct flag effects, plus add16 and addSP
- [x] Interrupt controller (IF/IE, IME, EI/DI delay, HALT wait-for-interrupt)
- [x] Master step loop services pending interrupts before fetch and ticks during HALT
- [x] Master step loop driving CPU, PPU, APU, timer, and DMA from a one-instruction budget (`Bus.advance` ticks all subsystems after each CPU
  instruction)
- [x] STOP instruction triggers CGB speed switch when KEY1 bit 0 is set; halts otherwise
- [x] HALT bug behavior with `IME = 0` and pending IF (latched in `Cpu.Execute.execute Halt` and consumed by the next `doInstruction` so the byte
  after `HALT` decodes twice)
- [x] Optional DMG/CGB boot ROM execution path: `ocelot play --boot-rom <path>` installs the boot ROM at `0x0000-0x00FF` (and `0x0200-0x08FF` on CGB)
  until the cartridge writes `0xFF50`. Without `--boot-rom`, falls back to the post-boot register state per platform.
- [x] Boot-ROM-aware peripheral power-on state: when a boot ROM is supplied, LCDC/BGP/OBP*/NR52 start at hardware power-on values (LCD off, palettes
  0, APU off) rather than post-boot defaults, so the boot ROM observes the same registers a real DMG/CGB does at reset
- [x] CGB double-speed mode (KEY1)

### Memory and Cartridge

- [x] DMG memory map: ROM banks, VRAM, ERAM, WRAM, OAM, IO, HRAM, IE
- [x] Echo RAM mirroring (`0xE000-0xFDFF` -> `0xC000-0xDDFF`)
- [x] OAM DMA (`0xFF46`) stepped one byte per M-cycle for 160 M-cycles, with non-HRAM CPU bus lockout while the transfer is active and a 1-cycle
  startup delay matching real hardware
- [x] CGB WRAM banking (`SVBK`/`WBK` at `0xFF70`, banks 1-7, bank 0 treated as bank 1)
- [x] CGB VRAM banking (`VBK` at `0xFF4F`, two 8 KiB banks)
- [x] CGB HDMA: general-purpose (instant copy) and HBlank (one 16-byte chunk per HBlank entry)
- [x] Cartridge header parsing (title, CGB flag, MBC type, ROM/RAM size, checksum)
- [x] No-MBC cartridges (32 KiB, optional 8 KiB RAM)
- [x] MBC1 with mode select (multicart variant detection deferred)
- [x] MBC2 (built-in 512x4-bit RAM, bit-8-of-address dispatch between RAM enable and ROM bank select)
- [x] MBC3 with RTC (POSIX-time backed live counter, halt, day-carry, latch sequence, RTC bank reads/writes)
- [x] MBC5 with bank switching (rumble bit not yet observable)
- [ ] MBC6, MBC7 (accelerometer and EEPROM)
- [x] HuC1 (RAM/IR mode select, 6-bit ROM bank, 2-bit RAM bank; IR transceiver itself is not modeled)
- [ ] HuC3, MMM01, and TAMA5
- [x] Battery-backed save persistence: `.sav` load and store on emulator entry/exit, RAM plus VBA-M-compatible 48-byte RTC suffix when the cart has a
  timer
- [x] General-purpose HDMA blocking timing: peripherals advance by `length / 2` M-cycles in single-speed (`length` in double-speed) during the copy, matching the 8 µs / 16 bytes Pan Docs spec; covered by the "advances peripherals during the copy block" regression in `Ocelot.CgbSpec`

### Timer and Serial

- [x] DIV register at 16384 Hz with reset-on-write
- [x] TIMA/TMA/TAC with selectable input clock
- [x] Timer falling-edge detector and TIMA reload window (writes to TIMA cancel reload, writes to TMA shift the loaded value, DIV/TAC writes that drop
  the AND signal increment TIMA). Mooneye timer category: 12/13 passing; `acceptance/timer/rapid_toggle.gb` is still pending.
- [x] Serial transfer (SB/SC) with stub clock for blargg test ROM output capture (writes to SC with bit 7 set capture SB to a buffer)
- [ ] Link cable peer mode (deferred; see Future Goals)

### Picture Processing Unit

- [x] LCDC, STAT, LY, LYC, SCX, SCY, WX, WY, BGP, OBP0, OBP1 register surface
- [x] Mode 2 OAM scan, Mode 3 pixel transfer, Mode 0 HBlank, Mode 1 VBlank state machine
- [x] Tile data fetch from `0x8000`/`0x8800` addressing modes
- [x] Background rendering (SCX fine-scroll discard still deferred)
- [x] Window rendering with a window-line counter
- [x] Sprite rendering (8x8 and 8x16) with DMG sort-by-X priority
- [x] STAT interrupt sources (LYC, mode 0/1/2) with edge-triggered IF latch
- [ ] Mid-scanline LCDC/SCX/WX changes reflected in mode 3 length
- [ ] Background pixel FIFO and sprite pixel FIFO with mid-line stalls
- [x] CGB BG and OBJ palette RAM (BCPS/BCPD/OCPS/OCPD) with auto-increment
- [x] CGB BG attribute byte (priority, V/H flip, VRAM bank, palette)
- [x] CGB sprite priority resolution (master priority bit, BG-to-OAM, OAM-order)
- [x] RGB framebuffer alongside the palette-index framebuffer (DMG via fixed shade palette, CGB via BG/OBJ palette RAM with RGB555 decoding)
- [ ] LCD on/off transitions and STAT/LY behavior on reset (LCD-off freeze is implemented; full reset semantics not audited)
- [x] Validation: dmg-acid2 golden frame hash (FNV-1a baseline locked; cross-check vs reference image at https://github.com/mattcurrie/dmg-acid2 to claim conformance)
- [x] Validation: cgb-acid2 golden frame hash (same caveat; baseline locked from current PPU output)
- [ ] Validation: mooneye PPU acceptance suite

### Audio Processing Unit

- [x] Channel 1: square wave with sweep, length, envelope
- [x] Channel 2: square wave with length, envelope
- [x] Channel 3: wave RAM playback with length, volume shift
- [x] Channel 4: LFSR noise with length, envelope (7-bit and 15-bit modes)
- [x] Frame sequencer at 512 Hz driving length, envelope, sweep events
- [x] Mixer (NR50, NR51) with per-side panning and master volume
- [x] Power register (NR52) with channel-enabled mirrors and write gating when off
- [x] DAC enable behavior
- [x] High-pass filter on the DAC output (RC capacitor model: leaky integrator removes DC offset, ~6 Hz cutoff at 48 kHz)
- [x] Sample resampler from 4 MHz tick rate to 48 kHz host audio rate
- [x] Bulk-step APU at event boundaries (per-T-cycle iteration replaced with chunked stepper)
- [x] Reusable APU sample queue with vector drains for frontend hot paths
- [ ] CGB stereo wave RAM read-during-play behavior
- [ ] Validation: blargg `dmg_sound` test ROMs
- [ ] Validation: blargg `cgb_sound` test ROMs

### Input and Interaction

- [x] Joypad register (P1) with row-select and active-low button matrix
- [x] Joypad interrupt on falling edge of any selected-row button bit
- [x] Keyboard input mapping in the SDL frontend (Z/X/Enter/Right-Shift + arrows + Escape)
- [x] Gamepad input mapping in the frontend
- [x] Frontend window with LCD framebuffer rendering (SDL2 RGB upload via a reusable staging buffer; terminal --headless mode also kept)
- [x] Integer scaling for SDL frontend (`--scale N`, 1–5; default 4) and web frontend (Auto/1×/2×/3×/4× in settings panel)
- [x] Audio output via SDL audio device with a callback-drained ring buffer
- [x] Desktop vsync-first renderer creation with sleep-based pacing fallback
- [x] Performance overlay with FPS and renderer pacing mode
- [x] In-memory snapshot save and load (`Ocelot.Snapshot.save`/`load`) with versioned binary format
- [x] Persistent save states: F5 saves, F7 loads; 5 slots (1-5) cycled with F6; files written to `<romdir>/<romstem>/slot<n>.state`
- [x] Screenshot capture: F12 writes a P6 PPM to `<romdir>/<romstem>/screenshot-<timestamp>.ppm`
- [x] GIF recording: Shift+F12 toggles capture; frames are palette-quantized and written to `<romdir>/<romstem>/recording-<timestamp>.gif`
- [x] Pause toggle (Space) and fast-forward (Tab held, 4x)
- [x] Paused and help-visible SDL loop waits on events or UI deadlines instead of forcing 60 Hz redraws
- [x] Frame step and reset hotkeys
- [x] Startup screen: native OS file picker on `> OPEN ROM` (zenity/kdialog on Linux, osascript on macOS, PowerShell on Windows) with drag-and-drop fallback; keyboard-navigable menu with `> OPEN ROM` and `QUIT`
- [x] Open ROM hotkey (O) while playing: saves battery, exits the current session, and returns to the startup screen
- [ ] PNG screenshot output (currently PPM)

### Save State Format

- [x] Versioned binary format (`OCS1` magic + LE u32 version, currently version 8)
- [x] CPU registers, IME, EI delay, halted, cycle counter
- [x] Bus WRAM, HRAM, IO, IE, WBK, KEY1
- [x] Bus HDMA src/dst/len/active and double-speed bits (v3 additions)
- [x] PPU registers, mode, dot, VRAM, OAM, palette-index framebuffer, VBK, BCPS, OCPS, BG palette RAM, OBJ palette RAM, window line, STAT edge state,
  and OPRI
- [x] APU full internal state (channels, frame sequencer, sample accumulator, wave RAM); sample queue is intentionally not snapshotted
- [x] Timer DIV, TIMA accumulator, TIMA, TMA, TAC
- [x] Joypad row-select, button bitmask, IRQ-pending latch
- [x] Cartridge external RAM, MBC bank state, MBC3 RTC live + latched

### Testing and Tooling

- [x] Hspec unit test root with `hspec-discover` auto-discovery
- [x] QuickCheck-based property tests for SM83 register pair and flag invariants
- [x] Module-local unit tests for CPU opcode groups, ALU, decoder
- [x] Module-local unit tests for cartridge header parsing, MBC banking, MBC3 RTC, save/load round-trip
- [x] PPU spec covering mode timing, BG/window/sprite rendering, register I/O
- [x] CGB spec covering banking, palette RAM I/O, BG/sprite rendering, priority, HDMA, double-speed
- [x] Snapshot round-trip spec for every subsystem
- [x] Integration test driving the public `Ocelot` facade through serial output
- [x] Regression test harness for ROM-backed runs in `test/Ocelot/GoldenSpec.hs`, gated on `OCELOT_GOLDEN=1`
- [x] Blargg test ROM collection wired in as a git submodule under `external/gb-test-roms`
- [x] Blargg cpu_instrs run-to-pass coverage: all 11 individual tests pass under `OCELOT_GOLDEN=1`
- [x] Blargg instr_timing run-to-pass coverage
- [x] Blargg result-via-`0xA000` memory runner: reads the final exit byte from cart RAM (per blargg's `shell.s`) so suites that don't print to
  serial (sound, oam_bug, mem_timing, halt_bug, interrupt_time) get a real verdict + numeric error code instead of "no Pass/Fail in N instructions"
  timeouts
- [x] Blargg mem_timing wired in (3 sub-ROMs, aspirational; reveals memory timing gaps via error codes)
- [x] Blargg dmg_sound wired in (12 sub-ROMs, aspirational; 8 currently pass: 01-registers, 02-len ctr, 03-trigger, 04-sweep, 05-sweep details,
  06-overflow on trigger, 08-len ctr during power, 11-regs after power)
- [x] Blargg cgb_sound wired in (12 sub-ROMs available, aspirational; 10 currently pass: 01-registers, 02-len ctr, 03-trigger, 04-sweep, 05-sweep
  details, 06-overflow on trigger, 08-len ctr during power, 10-wave trigger while on, 11-regs after power, 12-wave)
- [x] Blargg oam_bug wired in (8 sub-ROMs, aspirational; ~2 currently pass: 3-non_causes, 6-timing_no_bug)
- [x] Blargg halt_bug, interrupt_time wired in (aspirational; both currently report error code 0xFF)
- [ ] Promote aspirational blargg ROMs to strict run-to-pass as accuracy is added
- [x] Mooneye magic-breakpoint runner in `GoldenSpec.hs`: observes BCDEHL after each chunk for the Fibonacci pass tuple or all-`0x42` failure tuple
- [x] Mooneye prebuilt-ZIP fetcher (`make mooneye-roms`) downloads gekkio.fi's binaries to `test/testroms/mooneye/`
- [x] Mooneye acceptance ROMs auto-discovered and wired as one test per ROM (failures surface as `pendingWith` entries, not red marks, so the suite
  stays green while accuracy gaps are visible)
- [ ] mooneye acceptance category 100% run-to-pass coverage (current baseline is not locked in this document; `timer/` is 12/13, with
  `acceptance/timer/rapid_toggle.gb` pending)
- [ ] mooneye emulator-only category run-to-pass coverage
- [ ] mooneye CGB-specific category run-to-pass coverage
- [ ] Coverage reporting via `hpc-codecov` in CI
- [ ] Debugger: CPU step with register and flag display
- [ ] Debugger: memory hex dump viewer with bank navigation
- [ ] Debugger: VRAM tile and palette viewer
- [ ] Debugger: instruction-level breakpoints with run-to-breakpoint
- [ ] Debugger: disassembler view around PC

### Game Boy Color Support

- [x] CGB system detection from cartridge header CGB flag (`busCgb`)
- [x] Dual-mode (DMG/CGB) machine selection from header (`Ppu.setCgbMode`, `busCgb`)
- [x] CGB-only WRAM, VRAM, palette RAM, and HDMA paths
- [x] DMG-on-CGB compatibility palettes (3-state PPU render mode: `RenderDmg` / `RenderCgbCompat` / `RenderCgbFull`; CGB host pre-loads CGB palette
  RAM with a grayscale auto palette so DMG carts route through the CGB pipeline). Title-hash table for famous-title color sets is a follow-up.
- [x] DMG-on-CGB compatibility sprite priority: `RenderCgbCompat` sorts sprites by leftmost-X (matches the CGB boot ROM setting OPRI=1 for unmodified
  DMG carts), instead of CGB OAM-order priority.
- [x] Writable OPRI register (`0xFF6C`) for CGB carts that toggle sprite priority mid-game: bit 0 = 0 selects OAM-index priority, bit 0 = 1 selects
  leftmost-X priority. Bits 1-7 read as 1. Seeded to 1 by `Bus.fromCartridgeOnHost` for DMG-on-CGB compat carts and snapshotted (v8).
- [x] CGB double-speed switch (KEY1) timing for CPU-only subsystems (peripherals halved)
- [x] Validation: cgb-acid2 golden hash (FNV-1a baseline locked; cross-check vs Matt Currie's reference image before claiming conformance)
- [ ] Validation: mooneye CGB-specific acceptance tests

### Future Goals

- [ ] Link cable peer mode over a local socket (two-emulator multiplayer)
- [ ] Super Game Boy palette and border emulation
- [ ] Game Boy Printer peripheral (image capture sink)
- [ ] Game Boy Camera cartridge support
- [ ] Rewind functionality (per-frame state ring buffer)
- [ ] Configurable color correction (CGB LCD color profile, no-correction)
- [x] WebAssembly build with Canvas rendering and Web Audio playback (GHC WASM cross-compilation; browser host now uses RGBA framebuffer exports,
  direct `ImageData` blits, and transferable audio chunks)
- [ ] Libretro core packaging
- [ ] Cheat code support (Game Genie, GameShark)
- [ ] Lua or Haskell-script hookable trace API for tool-assisted runs

### Input Peripherals

- [ ] Standard Game Boy controller (built-in)
- [ ] Game Boy Printer
- [ ] Game Boy Camera
- [ ] Four Player Adapter (DMG-07)
- [ ] Mobile Adapter GB (network stub)
