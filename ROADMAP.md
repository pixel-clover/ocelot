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
- [x] Interrupt controller (IF/IE, IME, EI/DI delay, HALT wait-for-interrupt) — HALT bug and STOP behavior still placeholder
- [x] Master step loop services pending interrupts before fetch and ticks during HALT
- [ ] Master step loop driving CPU, PPU, APU, timer, and DMA from a one-clock budget — partial: CPU step calls Bus.advance after each instruction (currently a no-op)
- [ ] DMG boot ROM execution path (optional, with skip-to-post-boot fallback)
- [ ] CGB boot ROM execution path
- [ ] CGB double-speed mode (KEY1)

### Memory and Cartridge

- [x] DMG memory map: ROM banks, VRAM, ERAM, WRAM, OAM, IO, HRAM, IE
- [x] Echo RAM mirroring (`0xE000-0xFDFF` -> `0xC000-0xDDFF`)
- [x] OAM DMA (`0xFF46`) with instant copy (the 160-cycle delay and CPU-lockout deferred)
- [ ] CGB WRAM banking (`SVBK`, banks 1-7)
- [ ] CGB VRAM banking (`VBK`)
- [ ] CGB HDMA: general-purpose and HBlank transfers
- [x] Cartridge header parsing (title, CGB flag, MBC type, ROM/RAM size, checksum)
- [x] No-MBC cartridges (32 KiB, optional 8 KiB RAM)
- [x] MBC1 with mode select (multicart variant detection deferred)
- [ ] MBC2 (built-in 512x4-bit RAM)
- [x] MBC3 (without RTC; RTC reads return 0, RTC latch is a no-op)
- [x] MBC5 with bank switching (rumble bit not yet observable)
- [ ] MBC6, MBC7 (accelerometer, EEPROM)
- [ ] HuC1, HuC3, MMM01, TAMA5
- [ ] Battery-backed save persistence (`.sav` load and store)

### Timer and Serial

- [x] DIV register at 16384 Hz with reset-on-write
- [x] TIMA/TMA/TAC with selectable input clock
- [ ] Timer falling-edge detector and TIMA reload obscure behavior (deferred)
- [x] Serial transfer (SB/SC) with stub clock for blargg test ROM output capture (writes to SC with bit 7 set capture SB to a buffer)
- [ ] Link cable peer mode (deferred; see Future Goals)

### Picture Processing Unit

- [x] LCDC, STAT, LY, LYC, SCX, SCY, WX, WY, BGP, OBP0, OBP1 register surface
- [x] Mode 2 OAM scan, Mode 3 pixel transfer, Mode 0 HBlank, Mode 1 VBlank state machine
- [x] Tile data fetch from `0x8000`/`0x8800` addressing modes
- [x] Background rendering (SCX fine-scroll discard still deferred)
- [x] Window rendering (LY-WY approximation; proper window-row counter deferred)
- [x] Sprite rendering (8x8 and 8x16) with DMG priority rules
- [ ] STAT interrupt sources (LYC, mode 0/1/2) with edge-triggered IF latch
- [ ] Mid-scanline LCDC/SCX/WX changes reflected in mode 3 length
- [ ] Background pixel FIFO and sprite pixel FIFO with mid-line stalls
- [ ] CGB BG and OBJ palette RAM (BCPS/BCPD/OCPS/OCPD) with auto-increment
- [ ] CGB BG attribute byte (priority, V/H flip, VRAM bank, palette)
- [ ] CGB sprite priority resolution (master priority bit, BG-to-OAM)
- [ ] LCD on/off transitions and STAT/LY behavior on reset
- [ ] Validation: dmg-acid2 golden frame hash
- [ ] Validation: cgb-acid2 golden frame hash
- [ ] Validation: mooneye PPU acceptance suite

### Audio Processing Unit

- [x] Channel 1: square wave with sweep, length, envelope
- [x] Channel 2: square wave with length, envelope
- [x] Channel 3: wave RAM playback with length, volume shift
- [x] Channel 4: LFSR noise with length, envelope (7-bit and 15-bit modes)
- [x] Frame sequencer at 512 Hz driving length, envelope, sweep events
- [x] Mixer (NR50, NR51) with per-side panning and master volume
- [x] Power register (NR52) with channel-enabled mirrors and write gating when off
- [x] DAC enable behavior (high-pass capacitor model deferred)
- [x] Sample resampler from 4 MHz tick rate to 48 kHz host audio rate
- [ ] CGB stereo wave RAM read-during-play behavior
- [ ] Validation: blargg `dmg_sound` test ROMs
- [ ] Validation: blargg `cgb_sound` test ROMs

### Input and Interaction

- [x] Joypad register (P1) with row-select and active-low button matrix
- [ ] Joypad interrupt on falling edge
- [x] Keyboard input mapping in the SDL frontend (Z/X/Enter/Right-Shift + arrows + Escape)
- [ ] Gamepad input mapping in the frontend
- [x] Frontend window with LCD framebuffer rendering (SDL2; terminal --headless mode also kept)
- [x] Audio output via SDL audio device with callback-drained shared buffer
- [ ] In-memory quick save state and load
- [ ] Persistent save states (file-based serialization of CPU, MMU, PPU, APU, timer, cartridge state)
- [ ] Screenshot capture (PNG)
- [ ] Pause, frame step, and reset hotkeys

### Testing and Tooling

- [x] Hspec unit test root with `hspec-discover` auto-discovery
- [x] QuickCheck-based property tests for SM83 register pair and flag invariants
- [ ] Module-local unit tests for every CPU opcode group
- [ ] Module-local unit tests for MMU mirroring and MBC banking
- [ ] Integration test-driving the public `Ocelot` facade through one frame
- [ ] Regression test harness for ROM-backed golden hashes (`external/gb-test-roms/` for blargg, `test/testroms/` for custom ROMs)
- [x] Blargg test ROM collection wired in as a git submodule under `external/gb-test-roms`
- [x] Blargg cpu_instrs ROM run-to-pass coverage — all 11 individual tests pass (01-11)
- [ ] Blargg instr_timing ROM run-to-pass coverage
- [ ] Blargg mem_timing ROM run-to-pass coverage
- [ ] Blargg dmg_sound ROM run-to-pass coverage
- [ ] Blargg cgb_sound ROM run-to-pass coverage
- [ ] Blargg oam_bug ROM run-to-pass coverage
- [ ] Blargg halt_bug, interrupt_time ROM run-to-pass coverage
- [ ] mooneye-test-suite acceptance category run-to-pass coverage
- [ ] mooneye-test-suite emulator-only category run-to-pass coverage
- [ ] Coverage reporting via `hpc-codecov` in CI
- [ ] Debugger: CPU step with register and flag display
- [ ] Debugger: memory hex dump viewer with bank navigation
- [ ] Debugger: VRAM tile and palette viewer
- [ ] Debugger: instruction-level breakpoints with run-to-breakpoint
- [ ] Debugger: disassembler view around PC

### Game Boy Color Support

- [ ] CGB system detection from cartridge header CGB flag
- [ ] Dual-mode (DMG/CGB) machine selection from header
- [ ] CGB-only WRAM, VRAM, palette RAM, and HDMA paths
- [ ] DMG-on-CGB compatibility palettes (auto and manual selection)
- [ ] CGB double-speed switch (KEY1) timing for CPU-only subsystems
- [ ] Validation: cgb-acid2 golden hash
- [ ] Validation: mooneye CGB-specific acceptance tests

### Future Goals

- [ ] Link cable peer mode over a local socket (two-emulator multiplayer)
- [ ] Super Game Boy palette and border emulation
- [ ] Game Boy Printer peripheral (image capture sink)
- [ ] Game Boy Camera cartridge support
- [ ] Rewind functionality (per-frame state ring buffer)
- [ ] Fast-forward and frame-skip toggle
- [ ] Configurable color correction (CGB LCD color profile, no-correction)
- [ ] Integer scaling and pixel-perfect aspect ratio
- [ ] WebAssembly build with Canvas rendering and Web Audio playback
- [ ] Libretro core packaging
- [ ] Cheat code support (Game Genie, GameShark)
- [ ] Lua or Haskell-script hookable trace API for tool-assisted runs

### Input Peripherals

- [ ] Standard Game Boy controller (built-in)
- [ ] Game Boy Printer
- [ ] Game Boy Camera
- [ ] Four Player Adapter (DMG-07)
- [ ] Mobile Adapter GB (network stub)
