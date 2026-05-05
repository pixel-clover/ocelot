# AGENTS.md

This file provides guidance to coding agents collaborating on this repository.

## Mission

Ocelot is a portable and accurate Gameboy (DMG) and Gameboy Color (CGB) emulator written in Haskell.
Priorities, in order:

1. Correct emulation behavior and compatibility.
2. Clear timing and subsystem interactions (CPU/PPU/APU/timer/DMA/MBC).
3. Maintainable boundaries between frontend, library API, and core emulation.
4. Idiomatic, well-typed Haskell that the author can learn from.
5. Performance, but only after correctness is covered by tests.

This is also a learning project, so prefer designs that make Haskell concepts (pure functions, `ST`, `Data.Vector.Unboxed`, strictness, and
typeclasses) explicit and instructive over clever or terse alternatives.

## Core Rules

- Use English for code, comments, docs, and tests.
- Prefer small, focused changes over broad rewrites.
- Keep the project modular with separate the SM83 CPU, memory map (MMU), PPU, APU, timer, joypad, and cartridge/MBC into their own modules with clean
  APIs.
- Keep the emulator state explicit and instance-bound. A `Machine` (or per-subsystem `CpuState`, `PpuState`, etc.) record is the carrier; mutate via
  `ST`/`IORef`/`Data.Vector.Unboxed.Mutable` inside a subsystem, not via top-level globals.
- Avoid introducing a new global mutable state.
- Keep frontend/IO logic in `app/Main.hs` (and any future `app/Frontend/`); keep core emulation logic in `src/`.
- Use `Data.ByteString` for ROM and RAM byte buffers, `Data.Word` (`Word8`/`Word16`) for register and address types, and `Data.Vector.Unboxed` for
  tile/pixel buffers. Avoid `String` and `[Word8]` for hot paths.
- Add comments only when they clarify non-obvious hardware behavior, timing, or a Haskell-specific subtlety (laziness, `seq`, `ST` escape rules).
- Format with fourmolu (`make format`) and lint with HLint (`make lint`) before declaring a change done.

Quick examples:

- Good: add an SM83 opcode group inside `src/Ocelot/Cpu/Decode.hs` with module-local hspec tests.
- Good: add a Blargg/mooneye ROM-backed regression check under `test/Ocelot/GoldenSpec.hs` (gated on `OCELOT_GOLDEN=1`).
- Bad: move emulator core behavior into `app/Main.hs`.
- Bad: introduce a top-level `IORef` to share state between subsystems.

## Writing Style

- Use Oxford commas in inline lists: "a, b, and c" not "a, b, c".
- Do not use em dashes. Restructure the sentence, or use a colon or semicolon instead.
- Avoid colorful adjectives and adverbs. Write "instruction decoder" not "elegant instruction decoder".
- Use noun phrases for checklist items, not imperative verbs. Write "opcode timing table" not "build the opcode timing table".
- Headings in Markdown files must be in title case: "Build from Source" not "Build from source". Minor words (a, an, the, and, but, or, for, in, on,
  at, to, by, of) stay lowercase unless they are the first word.

## Repository Layout

The current tree is small; this layout describes the target structure as the project grows.
Do not invent modules that do not yet exist when answering questions, but do place new modules according to this map.

- `app/Main.hs`: executable entry point. Argument parsing, ROM loading from disk, headless terminal mode, and the SDL frontend dispatch live here.
- `app/Frontend/Sdl.hs`: SDL2-backed frontend with video, audio, and hotkeys (pause, fast-forward, save state, load state, screenshot).
- `src/`: library code. Public API root is `Ocelot` (re-exports the curated public surface).
    - `src/Ocelot.hs`: public facade. Re-exports the deliberate public types (`Cartridge`, header records, save helpers). Do not re-export raw
      subsystem state records.
    - `src/Ocelot/Machine.hs`: top-level `Machine` record stitching CPU and bus together; CPU step lives in `Ocelot.Cpu.Execute`.
    - `src/Ocelot/Cpu/`: SM83 CPU. Registers, flags, decoder, executor, interrupts.
    - `src/Ocelot/Cartridge.hs` and `src/Ocelot/Cartridge/`: ROM header parsing, MBC variant implementations, battery save handling, MBC3 RTC.
    - `src/Ocelot/Ppu.hs`: pixel pipeline, OAM scan, BG/window/sprite fetch, mode state machine, DMG and CGB palettes, RGB framebuffer.
    - `src/Ocelot/Apu.hs`: four channels, frame sequencer, mixer, sample resampler.
    - `src/Ocelot/Timer.hs`: DIV, TIMA, TMA, TAC.
    - `src/Ocelot/Joypad.hs`: P1 register, button matrix, joypad-IRQ edge detection.
    - `src/Ocelot/Bus.hs`: cross-subsystem read/write coordination, address decoding, WRAM/HRAM, OAM DMA, CGB HDMA, CGB banking, KEY1 + double-speed
      tick scaling. Memory work that does not belong to a peripheral lives here, not in a separate `Memory` module.
    - `src/Ocelot/Snapshot.hs` and `src/Ocelot/Snapshot/Binary.hs`: versioned save-state format with put/get primitives.
    - `src/Ocelot/Testing.hs`: deliberate testing facade for low-level access.
- `test/`: Hspec suite. `Spec.hs` is the `hspec-discover` entry; per-module specs live alongside as `Ocelot/<Module>Spec.hs`. Cross-cutting specs are
  `IntegrationSpec`, `GoldenSpec` (ROM-driven, gated on `OCELOT_GOLDEN=1`), `CgbSpec`, and `SnapshotSpec`.
- `test/testroms/`: third-party and custom test ROMs the regression suite reads at runtime. Hand-authored regression ROMs are
  tracked here; downloaded artifacts (mooneye, acid2) are gitignored and fetched with `make test-roms`. Layout:
    - `test/testroms/mooneye/`: prebuilt mooneye-test-suite ROMs from gekkio.fi (`make mooneye-roms`).
    - `test/testroms/dmg-acid2.gb`: Matt Currie's DMG PPU acid2 (`make acid2-roms`).
    - `test/testroms/cgb-acid2.gbc`: Matt Currie's CGB PPU acid2 (`make acid2-roms`).
- `external/`: third-party source trees pulled in as git submodules. Initialize with `git submodule update --init --recursive`.
    - `external/gb-test-roms/`: blargg test ROM collection from `retrio/gb-test-roms`. Load-bearing for the cpu_instrs,
      instr_timing, mem_timing, dmg_sound, cgb_sound, oam_bug, halt_bug, and interrupt_time regression coverage. The `.gb`
      files live in the submodule and are read directly.
- `docs/`: project documentation and Haddock output target (`docs/haskell/`).
- `Makefile`: developer workflow entry points (`build`, `test`, `lint`, `format`, `format-check`, `coverage`, `doc`, and `repl`).
- `package.yaml`: hpack source of truth. Do not hand-edit `*.cabal`; let `stack build` regenerate it.
- `stack.yaml`: resolver pin and packages.

## Testing Layout Rules

- Unit tests for module `Ocelot.Foo.Bar` belong in `test/Ocelot/Foo/BarSpec.hs` and are auto-discovered by `hspec-discover` via `test/Spec.hs`.
- Cross-subsystem tests belong in `test/Ocelot/IntegrationSpec.hs`. ROM-driven golden tests belong in `test/Ocelot/GoldenSpec.hs` and must be
  gated on `OCELOT_GOLDEN=1` so default `stack test` stays fast.
- Property-based tests (QuickCheck) belong with the unit spec for the module whose invariants they exercise.
- Non-unit tests should drive emulation through the `Ocelot` public facade rather than reaching into `Ocelot.Cpu.Internal` etc. If they need
  lower-level control, add a deliberate testing facade in `src/Ocelot/Testing.hs` rather than re-exporting raw state.
- ROM-dependent tests belong in `test/Ocelot/GoldenSpec.hs` and must skip cleanly when the ROM file is absent (so a fresh checkout without
  `git submodule update --init` still passes), and must additionally pend with a clear hint when `OCELOT_GOLDEN` is not set.
- Blargg ROM-backed checks read from `external/gb-test-roms/`. Mooneye, acid2, and any other downloaded or custom test ROMs
  live under `test/testroms/`.
- If you move code across modules, move or rewrite the unit tests with it.

## Architecture Constraints

- The `Machine` record (`src/Ocelot/Machine.hs`) is the central coordination point: it pairs an `IORef CpuState` with a `Bus`. The bus then
  carries every other subsystem's state.
- One canonical step path advances time. `Ocelot.Cpu.Execute.step` runs one CPU instruction (services pending interrupts, fetches and executes
  one opcode, ticks bus subsystems by the consumed M-cycles via `Bus.advance`). Add new timing behavior to that path; do not introduce parallel
  scheduler entry points.
- Timing-sensitive changes must respect the interaction between:
    - `Ocelot.Cpu.Execute.step` (M-cycle accounting, interrupt servicing, calls `Bus.advance`)
    - `Bus.read8` / `Bus.write8` (PPU mode and OAM/VRAM gating, MBC routing)
    - `Bus.advance` (peripheral cycle dispatch; halves the cycle count for peripherals in CGB double-speed mode)
    - `Ppu.advance` (mode 2/3/0/1 transitions, STAT/VBlank interrupts, HBlank-entered signal for HDMA)
    - `Timer.advance` (DIV/TIMA edges, TAC obscure behavior)
    - `Apu.advance` (frame sequencer steps tied to DIV)
- Cartridge MBC behavior is owned by `Ocelot.Cartridge`. The bus calls into the cartridge for `0x0000-0x7FFF` and `0xA000-0xBFFF`; do not bypass it
  from elsewhere.
- Keep frontend concerns (like windowing, audio output device, key mapping concrete codes, etc.) separate from emulation concerns.

## Component APIs

Each subsystem (`Cpu`, `Ppu`, `Apu`, `Timer`, `Joypad`, and `Cartridge`) owns its own state record and exposes a narrow function-level API.
Other subsystems and the bus interact through these functions only; they do not poke each other's `IORef`s or `IOVector`s directly (with two narrow
exceptions called out below: PpuState fields are exported so `Bus` can route memory windows, and the Snapshot module reaches into PpuState and
BusState for save/load). The bus is the one place that knows the full address map.

The signatures below describe the actual public surface; the project landed on an `IO`-based architecture (each `*State` carries `IORef`s and
mutable `IOVector`s) rather than the original pure `(state -> (a, state))` aspiration, because per-T-cycle state-threading was both unergonomic and
slower than direct mutation. New subsystem code should follow the same pattern.

### `Ocelot.Bus`

Cross-subsystem read/write coordination, plus M-cycle dispatch.

- `read8 :: Word16 -> Bus -> IO Word8`
- `write8 :: Word16 -> Word8 -> Bus -> IO ()`
- `advance :: Int -> Bus -> IO ()` (M-cycles; ticks Timer, PPU, APU, OAM DMA, HDMA HBlank step, joypad IRQ edge in lockstep, halving peripheral
  cycles in CGB double-speed mode)
- `triggerSpeedSwitch :: Bus -> IO Bool` (called from the CPU's `STOP` handler)

Bus is the only place that knows the full address map: it dispatches `0x0000-0x7FFF` and `0xA000-0xBFFF` to the cartridge, the VRAM/OAM windows
to the PPU, the audio register windows to the APU, IO/HRAM/IE to its own buffers, and the CGB extension registers (VBK, BCPS/BCPD, OCPS/OCPD,
WBK, KEY1, HDMA1-5) to the right peer.

### `Ocelot.Cpu`

- `Ocelot.Cpu.Execute.step :: Machine -> IO ()` (one instruction; reads/writes go through `Bus`; cycle accounting is stored on the CPU state)
- `Ocelot.Cpu.Execute.runFor :: Int -> Machine -> IO Int` and `runUntilHalt :: Int -> Machine -> IO Int` (test/headless helpers)
- Interrupt servicing is folded into `step`; there is no separately exposed entry point.

CPU never imports `Ocelot.Ppu`, `Ocelot.Apu`, `Ocelot.Timer`, or `Ocelot.Cartridge`. Memory access goes through `Bus`. The single `Ocelot.Bus`
import inside `Cpu.Execute` is for `triggerSpeedSwitch` (the `STOP` instruction) and is the only cross-subsystem coupling outside the bus.
Reading or writing CPU registers from outside `Ocelot.Cpu` is allowed only for tests; production code does not poke `regA`, `regPC`, etc.

### `Ocelot.Ppu`

- Memory windows and registers: `read8 :: Word16 -> PpuState -> IO Word8`, `write8 :: Word16 -> Word8 -> PpuState -> IO ()` (covers VRAM, OAM,
  the LCDC/STAT register surface at `0xFF40-0xFF4B`, plus CGB-only `0xFF4F`, `0xFF68-0xFF6C`)
- Time advance: `advance :: Int -> PpuState -> IO Word8` (returns a flag bitmask: bit 0 = VBlank IRQ, bit 1 = STAT IRQ, bit 2 = HBlank-entered
  for HDMA stepping)
- Framebuffer accessors: `framebuffer :: PpuState -> IO (Vector Word8)` (DMG palette indices) and `framebufferRgb :: PpuState -> IO (Vector Word8)`
  (RGB888 bytes; what the SDL frontend uses)
- CGB hookup: `setCgbMode :: Bool -> PpuState -> IO ()` (called by the bus once at startup)
- CGB render-mode hookup: `setCgbRenderMode :: CgbRenderMode -> PpuState -> IO ()`
- STAT write-edge hookup: `takePendingStatIrq :: PpuState -> IO Bool` (called by the bus after PPU register writes that can raise STAT)

`PpuState` exports its field record so `Bus` can route memory accesses and so `Snapshot` can serialize the IORefs and IOVectors directly. Treat
the surface listed above as the contract; do not call other PpuState fields from outside `Ocelot.Ppu` outside Snapshot.

### `Ocelot.Apu`

- Register I/O: `read8 :: Word16 -> ApuState -> IO Word8`, `write8 :: Word16 -> Word8 -> ApuState -> IO ()` (covers `0xFF10-0xFF26` and the wave
  RAM at `0xFF30-0xFF3F`)
- Time advance: `advance :: Int -> ApuState -> IO ()` (queues stereo samples; the bus drains them)
- Sample drain: `drainSamples :: ApuState -> IO [Int16]`
- CGB hookup: `setCgbMode :: Bool -> ApuState -> IO ()`
- Host sample rate: `sampleRate :: Int`
- Snapshot hooks: `dumpState :: ApuState -> IO ByteString`, `loadState :: ByteString -> ApuState -> IO ()`

`ApuState` is exported as an opaque type; the channel and frame-sequencer types stay internal.

### `Ocelot.Timer`

- `TimerState` is exported as a record (DIV, TIMA accumulator, TIMA, TMA, TAC fields are part of the API).
- Pure register I/O: `readDiv`, `readTima`, `readTma`, `readTac`, `writeDiv`, `writeTima`, `writeTma`, `writeTac`.
- Pure time advance: `advance :: Int -> TimerState -> (TimerState, Bool)` (returns `True` when TIMA overflowed at least once).

The timer is the one peripheral that is still pure. The bus owns the `IORef TimerState` and threads the new state back after each advance.

### `Ocelot.Joypad`

- `setButton :: Button -> Bool -> JoypadState -> IO ()` (frontend pushes input; latches an IRQ edge on a falling-bit transition)
- `readP1 :: JoypadState -> IO Word8`, `writeP1 :: Word8 -> JoypadState -> IO ()`
- `takeIrqPending :: JoypadState -> IO Bool` (consumed by `Bus.advance`)
- Snapshot hooks: `dumpState :: JoypadState -> IO (Word8, Word8, Bool)`, `loadState :: (Word8, Word8, Bool) -> JoypadState -> IO ()`

`JoypadState` is exported as an opaque type; the frontend never touches its fields.

### `Ocelot.Cartridge`

- `read8 :: Word16 -> Cartridge -> IO Word8` (covers `0x0000-0x7FFF` and `0xA000-0xBFFF`)
- `write8 :: Word16 -> Word8 -> Cartridge -> IO ()`
- `loadRom :: ByteString -> IO (Either CartridgeError Cartridge)`
- Save handling: `loadSave :: ByteString -> Cartridge -> IO ()`, `extractSave :: Cartridge -> IO ByteString`, `cartridgeHasBattery`,
  `extractRam`/`loadRam`
- Snapshot hooks: `dumpMbc :: Cartridge -> IO ByteString`, `loadMbc :: ByteString -> Cartridge -> IO ()` (MBC bank-select state)

MBC variant selection (no-MBC, MBC1, MBC2, MBC3 with RTC, MBC5, and HuC1) is internal. The bus sees only `read8` and `write8`. RTC persistence uses
the
VBA-M-compatible 48-byte suffix appended to the RAM bytes in `extractSave`/`loadSave`.

### `Ocelot.Snapshot`

- `save :: Machine -> IO ByteString` and `load :: ByteString -> Machine -> IO (Either SnapshotError ())`
- Versioned binary format (`OCS1` magic + LE u32 version, currently 8). When the format changes incompatibly, bump the version; old blobs are
  rejected with `UnsupportedVersion`.
- Reaches across subsystems via the per-module `dumpState`/`loadState` hooks listed above and via direct PpuState/Bus field access where the
  state is in IORefs and IOVectors that the per-module hooks would just wrap.

### Interrupt Latching

The bus owns IF (`0xFF0F`). Subsystems do not write to it directly; instead `Ppu.advance` and `Timer.advance` return flag information, and
`Joypad` exposes a one-shot `takeIrqPending`. `Bus.advance` is the single place that latches those edges into IF after each CPU instruction.

### Encapsulation Rule

A subsystem's `*State` type (e.g. `JoypadState`, `ApuState`) is exported as an opaque type from its module wherever the implementation can hide
its fields. The current intentional exceptions, where the field record is exported, are:

- `Ocelot.Cpu.Registers.Registers (..)`: a leaf data type with no internal state machine, exported in full because there is nothing to hide and
  the surrounding module enforces the F-register low-nibble invariant through smart accessors.
- `Ocelot.Cpu.State.CpuState (..)` and `Ocelot.Timer.TimerState (..)`: small flat records with no invariants beyond field types.
- `Ocelot.Ppu.PpuState (..)` and `Ocelot.Bus.Bus (..)`: exported because their `IORef`/`IOVector` fields are written directly by the bus router
  and the snapshot module. The `read8`/`write8`/`advance` surface is still the right way to drive these from outside.
- `Ocelot.Testing` is the deliberate testing facade for low-level access. Do not add a "just for now" re-export anywhere else.

## Workflow

Before coding:

1. Identify whether this is a CPU/timing, memory/MBC, PPU, APU, frontend, or docs change.
2. Read the touched module and existing nearby tests.

Implement using red-green TDD:

1. Write a failing hspec test first that describes the expected behavior (red). For per-instruction or per-flag work, prefer a QuickCheck property
   when an invariant exists ("`add a 0 == a`", "`SUB` then `ADD` round-trips", etc.).
2. Run the test and verify it fails for the right reason: `stack test --test-arguments "--match \"<spec name>\""`.
3. Write the smallest implementation that makes the test pass (green).
4. Refactor while keeping tests green.
5. Run the narrowest relevant spec while iterating, then `make test` and `make lint` before declaring done.
6. Run `make format` (or `make format-check` in CI).
7. Update docs (`README.md`, `docs/`, Haddock on the public facade) if behavior or workflow changed.

Additional validation when relevant:

- `make doc` for Haddock changes on the public API.
- `make coverage` when adding or restructuring tests; check `.stack-work/install/*/hpc/`.
- `make repl` (`stack ghci`) for ad-hoc exploration; do not commit REPL-only helpers.
- `stack run -- <path-to-rom>` for frontend or end-to-end manual checks.

Optimize-mode guidance:

- Default development uses the standard `stack build` (unoptimized, fast rebuilds).
- Use `make release` (`-O2`) for ROM-backed performance checks or long gameplay runs; do not benchmark unoptimized builds.
- Keep unoptimized builds for stepping, tracing, and assertion-heavy debugging.

## Testing Expectations

- No emulation behavior change is complete without tests.
- CPU instructions, flag effects, MMU mirroring, MBC banking, PPU mode timing, timer edges, and interrupt dispatch all need explicit coverage.
- Prefer targeted assertions (one register, one flag, one cycle count) over broad snapshot tests, unless the behavior is naturally end-to-end (e.g. a
  Blargg ROM run-to-pass).
- Keep tests deterministic. Initialize only the state you need, drive the public API, and assert on observable behavior.
- When uncertain about emulator correctness, add or refine tests first.

## Documentation Expectations

- Public-facing API docs are generated from Haddock on `src/Ocelot.hs`. Keep that module focused on deliberate public surfaces; do not re-export raw
  internal coordination types like `CpuState`, `PpuState`, or `Bus`. Add facade or view types instead.
- User workflow changes should update `README.md`.
- Progress and completeness changes should update `ROADMAP.md`.
- If you detect stale docs while changing related code, fix them in the same patch.

## Review Guidelines (P0/P1 Focus)

Review output should be concise and only include critical issues.

- `P0`: must-fix defects (incorrect emulation behavior, severe regression, broken build or test workflow).
- `P1`: high-priority defects (like possible timing bug, incorrect subsystem coupling, missing validation for a risky change).

Use this review format:

1. `Severity` (`P0`/`P1`)
2. `File:line`
3. `Issue`
4. `Why it matters`
5. `Minimal fix direction`

Do not include style-only feedback or broad praise.
