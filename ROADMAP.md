## Project Roadmap

This document outlines the features implemented in the Ocelot emulator, and the future goals for the project.

> [!IMPORTANT]
> This roadmap is a work in progress and is subject to change.

### Core System

- [x] Project scaffolding (Stack, hpack, fourmolu, HLint, flake-based dev shell)
- [x] Public API facade module (`Ocelot`) with versioned entry point
- [x] SM83 register file with 8-bit registers, 16-bit pairs, and Z/N/H/C flag accessors
- [x] DMG post-boot register state constant
- [ ] SM83 unprefixed opcode decoder (256 entries) with M-cycle timing
- [ ] SM83 CB-prefix opcode decoder (256 entries) with M-cycle timing
- [ ] ALU helpers (add/adc/sub/sbc/and/or/xor/cp/inc/dec) with correct flag effects
- [ ] Bit/rotate/shift helpers (RLC/RRC/RL/RR/SLA/SRA/SWAP/SRL/BIT/RES/SET)
- [ ] Interrupt controller (IF/IE, IME, EI/DI semantics, HALT bug, STOP)
- [ ] Master step loop driving CPU, PPU, APU, timer, and DMA from one clock budget
- [ ] DMG boot ROM execution path (optional, with skip-to-post-boot fallback)
- [ ] CGB boot ROM execution path
- [ ] CGB double-speed mode (KEY1)

### Memory and Cartridge

- [ ] DMG memory map: ROM banks, VRAM, ERAM, WRAM, OAM, IO, HRAM, IE
- [ ] Echo RAM mirroring (`0xE000-0xFDFF` -> `0xC000-0xDDFF`)
- [ ] OAM DMA (`0xFF46`) with 160-cycle copy and CPU access restrictions
- [ ] CGB WRAM banking (`SVBK`, banks 1-7)
- [ ] CGB VRAM banking (`VBK`)
- [ ] CGB HDMA: general-purpose and HBlank transfers
- [ ] Cartridge header parsing (title, CGB flag, MBC type, ROM/RAM size, checksum)
- [ ] No-MBC cartridges (32 KiB, optional 8 KiB RAM)
- [ ] MBC1 with mode select, multicart variant detection
- [ ] MBC2 (built-in 512x4-bit RAM)
- [ ] MBC3 with RTC latching and battery-backed time
- [ ] MBC5 with rumble bit
- [ ] MBC6, MBC7 (accelerometer, EEPROM)
- [ ] HuC1, HuC3, MMM01, TAMA5
- [ ] Battery-backed save persistence (`.sav` load and store)

### Timer and Serial

- [ ] DIV register at 16384 Hz with reset-on-write
- [ ] TIMA/TMA/TAC with selectable input clock
- [ ] Timer falling-edge detector and TIMA reload obscure behavior
- [ ] Serial transfer (SB/SC) with stub clock for blargg test ROM output capture
- [ ] Link cable peer mode (deferred; see Future Goals)

### Picture Processing Unit

- [ ] LCDC, STAT, LY, LYC, SCX, SCY, WX, WY, BGP, OBP0, OBP1 register surface
- [ ] Mode 2 OAM scan, Mode 3 pixel transfer, Mode 0 HBlank, Mode 1 VBlank state machine
- [ ] Tile data fetch from `0x8000`/`0x8800` addressing modes
- [ ] Background and window rendering with SCX fine-scroll discard
- [ ] Sprite rendering (8x8 and 8x16) with DMG priority rules
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

- [ ] Channel 1: square wave with sweep, length, envelope
- [ ] Channel 2: square wave with length, envelope
- [ ] Channel 3: wave RAM playback with length, volume shift
- [ ] Channel 4: LFSR noise with length, envelope
- [ ] Frame sequencer at 512 Hz driving length, envelope, sweep events
- [ ] Mixer (NR50, NR51) with per-side panning and master volume
- [ ] Power register (NR52) with channel enable mirrors and write gating when off
- [ ] DAC enable behavior and high-pass capacitor model
- [ ] Sample resampler from 1.048 MHz tick rate to host audio rate
- [ ] CGB stereo wave RAM read-during-play behavior
- [ ] Validation: blargg `dmg_sound` test ROMs
- [ ] Validation: blargg `cgb_sound` test ROMs

### Input and Interaction

- [ ] Joypad register (P1) with directional and action button selection
- [ ] Joypad interrupt on falling edge
- [ ] Keyboard input mapping in the frontend
- [ ] Gamepad input mapping in the frontend
- [ ] Frontend window with LCD framebuffer rendering (terminal first, then SDL)
- [ ] Audio output via SDL audio device with ring-buffered samples
- [ ] In-memory quick save state and load
- [ ] Persistent save states (file-based serialization of CPU, MMU, PPU, APU, timer, cartridge state)
- [ ] Screenshot capture (PNG)
- [ ] Pause, frame step, and reset hotkeys

### Testing and Tooling

- [x] Hspec unit test root with `hspec-discover` auto-discovery
- [x] QuickCheck-based property tests for SM83 register pair and flag invariants
- [ ] Module-local unit tests for every CPU opcode group
- [ ] Module-local unit tests for MMU mirroring and MBC banking
- [ ] Integration test driving the public `Ocelot` facade through one frame
- [ ] Regression test harness for ROM-backed golden hashes (`test/testroms/`)
- [ ] Blargg cpu_instrs ROM run-to-pass coverage
- [ ] Blargg instr_timing ROM run-to-pass coverage
- [ ] Blargg mem_timing ROM run-to-pass coverage
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

- [ ] Link cable peer mode over local socket (two-emulator multiplayer)
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
