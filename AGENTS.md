# AGENTS.md

This file provides guidance to coding agents collaborating on this repository.

## Mission

Ocelot is a portable and accurate Game Boy (DMG) and Game Boy Color (CGB) emulator written in Haskell.
Priorities, in order:

1. Correct emulation behavior and compatibility.
2. Clear timing and subsystem interactions (CPU/PPU/APU/timer/DMA/MBC).
3. Maintainable boundaries between frontend, library API, and core emulation.
4. Idiomatic, well-typed Haskell that the author can learn from.
5. Performance, but only after correctness is covered by tests.

This is also a learning project: prefer designs that make Haskell concepts (pure functions, `ST`, `Data.Vector.Unboxed`, strictness, typeclasses)
explicit and instructive over clever or terse alternatives.

## Core Rules

- Use English for code, comments, docs, and tests.
- Prefer small, focused changes over broad rewrites.
- Keep the project modular: separate the SM83 CPU, memory map (MMU), PPU, APU, timer, joypad, and cartridge/MBC into their own modules with clean
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
- Good: add a Blargg/mooneye ROM-backed regression check under `test/Ocelot/RegressionSpec.hs`.
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

The current tree is small; this layout describes the target structure as the project grows. Do not invent modules that do not yet exist when answering
questions, but do place new modules according to this map.

- `app/Main.hs`: executable entry point. Argument parsing, ROM loading from disk, and (eventually) the SDL or terminal frontend live here.
- `src/`: library code. Public API root is `Ocelot` (re-exports the curated public surface).
    - `src/Ocelot.hs`: public facade. Re-exports the deliberate public types (`Machine`, `Cartridge`, `step`, `runFrame`). Do not re-export raw
      subsystem state records.
    - `src/Ocelot/Machine.hs`: top-level `Machine` record stitching CPU, MMU, PPU, APU, timer, joypad, cartridge together; frame stepping.
    - `src/Ocelot/Cpu/`: SM83 CPU. Registers, flags, decoder, executor, interrupts.
    - `src/Ocelot/Memory/`: MMU, address decoding, OAM DMA, HDMA (CGB), WRAM/HRAM.
    - `src/Ocelot/Cartridge/`: ROM header parsing, MBC0/1/2/3/5 implementations, battery-backed save handling.
    - `src/Ocelot/Ppu/`: pixel pipeline, OAM scan, BG/window/sprite fetch, mode/state machine, CGB palettes.
    - `src/Ocelot/Apu/`: four channels, frame sequencer, mixer.
    - `src/Ocelot/Timer/`: DIV/TIMA/TMA/TAC, obscure behavior.
    - `src/Ocelot/Joypad/`: P1 register, input mapping (frontend wires concrete keys).
    - `src/Ocelot/Bus.hs`: cross-subsystem read/write coordination (PPU mode gating, OAM/VRAM access blocking, MBC routing).
- `test/`: Hspec suite. `Spec.hs` is the `hspec-discover` entry; per-module specs live alongside as `Ocelot/<Module>Spec.hs`.
- `test/testroms/`: small custom or non-blargg test ROMs (mooneye snippets, dmg-acid2, ad-hoc snippets) used by regression specs. May be absent.
- `external/`: third-party source trees pulled in as git submodules.
    - `external/gb-test-roms/`: blargg test ROM collection (`gb-test-roms-fork`, branch `master`). Load-bearing for the regression suite. Initialize
      with `git submodule update --init --recursive`.
- `roms/`: local game ROMs for manual testing. Always gitignored. May be absent.
- `docs/`: project documentation and Haddock output target (`docs/haskell/`).
- `Makefile`: developer workflow entry points (`build`, `test`, `lint`, `format`, `format-check`, `coverage`, `doc`, `repl`).
- `package.yaml`: hpack source of truth. Do not hand-edit `*.cabal`; let `stack build` regenerate it.
- `stack.yaml`: resolver pin and packages.

## Testing Layout Rules

- Unit tests for module `Ocelot.Foo.Bar` belong in `test/Ocelot/Foo/BarSpec.hs` and are auto-discovered by `hspec-discover` via `test/Spec.hs`.
- Cross-subsystem and ROM-backed regression tests belong in `test/Ocelot/IntegrationSpec.hs` and `test/Ocelot/RegressionSpec.hs`.
- Property-based tests (QuickCheck) belong with the unit spec for the module whose invariants they exercise.
- Non-unit tests should drive emulation through the `Ocelot` public facade rather than reaching into `Ocelot.Cpu.Internal` etc. If they need
  lower-level control, add a deliberate testing facade in `src/Ocelot/Testing.hs` rather than re-exporting raw state.
- ROM-dependent tests belong in `test/Ocelot/RegressionSpec.hs` and must skip cleanly when the ROM file is absent (so a fresh checkout without
  `git submodule update --init` still passes).
- Blargg ROM-backed checks read from `external/gb-test-roms/`. Custom test ROMs (mooneye snippets, dmg-acid2, ad-hoc snippets) live under
  `test/testroms/`.
- If you move code across modules, move or rewrite the unit tests with it.

## Architecture Constraints

- The `Machine` record is the central coordination point for memory, timing, and subsystem progress.
- One canonical step path advances time. Initially `Ocelot.Machine.step` runs one CPU instruction and consumes the resulting M-cycles across PPU, APU,
  timer, and DMA. Add new timing behavior to that path; do not introduce parallel scheduler entry points.
- Timing-sensitive changes must respect the interaction between:
    - `Cpu.stepInstruction` (M-cycle accounting)
    - `Bus.read8` / `Bus.write8` (PPU mode and OAM/VRAM gating, MBC routing)
    - `Ppu.advance` (mode 2/3/0/1 transitions, STAT/LY interrupts)
    - `Timer.advance` (DIV/TIMA edges, TAC obscure behavior)
    - `Apu.advance` (frame sequencer steps tied to DIV)
- Cartridge MBC behavior is owned by `Ocelot.Cartridge`. The bus calls into the cartridge for `0x0000–0x7FFF` and `0xA000–0xBFFF`; do not bypass it
  from elsewhere.
- Keep frontend concerns (windowing, audio output device, key mapping concrete codes) separate from emulation concerns.
- Preserve MIT-license boundaries. Treat reference emulators (SameBoy, mooneye-gb, etc.) as references for behavior, not as code to copy. Test ROMs
  are licensed by their authors. For blargg ROMs under `external/gb-test-roms/`, the upstream `readme.txt` is authoritative. For custom ROMs added
  under `test/testroms/`, record provenance in `test/testroms/README.md`.

## Component APIs

Each subsystem (`Cpu`, `Ppu`, `Apu`, `Timer`, `Joypad`, `Cartridge`, `Memory`) owns its own state record and exposes a narrow function-level API.
Other subsystems and the bus interact through these functions only; they do not read state-record fields directly. This is the rule that turns "
modular" from an aspiration into a property the compiler enforces: each `*State` type is exported as opaque, so cross-module coupling must go through
the listed surface.

The signatures below are targets for modules that do not yet exist. They define the *shape* of each public surface (what is exposed, what stays
hidden); exact types will refine as code lands. When implementing a module, expose at least these functions; additional helpers are fine, but the
listed surface is the contract that other subsystems and the bus depend on.

### `Ocelot.Bus`

Cross-subsystem read/write coordination, plus M-cycle dispatch.

- `read8 :: Word16 -> Machine -> (Word8, Machine)`
- `write8 :: Word16 -> Word8 -> Machine -> Machine`
- `advance :: MCycles -> Machine -> Machine` (steps PPU, APU, Timer, OAM DMA in lockstep)

Bus is the only place that knows the full address map. It satisfies each access by calling subsystem APIs (`Ppu.canAccessVram`, `Cartridge.read8`,
etc.); it does not inspect their state-record fields.

### `Ocelot.Cpu`

- `stepInstruction :: Machine -> (Machine, MCycles)` (one instruction; reads/writes through `Bus`)
- `serviceInterrupts :: Machine -> (Machine, MCycles)` (consults `IF` and `IE` via `Bus`, dispatches to handler)

CPU never imports `Ocelot.Ppu`, `Ocelot.Apu`, `Ocelot.Timer`, or `Ocelot.Cartridge`. Memory access goes through `Bus`. Reading or writing CPU
registers from outside `Ocelot.Cpu` is allowed only for tests; production code does not poke `regA`, `regPC`, etc.

### `Ocelot.Ppu`

- Access gating predicates: `canAccessVram :: PpuState -> Bool`, `canAccessOam :: PpuState -> Bool` (return `False` during mode 2 and mode 3)
- Memory windows: `readVram` / `writeVram`, `readOam` / `writeOam`
- Register I/O: `readReg :: PpuReg -> PpuState -> Word8`, `writeReg :: PpuReg -> Word8 -> PpuState -> PpuState`
- Time advance: `advance :: MCycles -> PpuState -> (PpuState, [Interrupt])`

`PpuState` is exported as an opaque type. Field accessors stay internal. The two predicates exist specifically so `Bus` can gate VRAM/OAM access
without reading PPU mode bits directly.

### `Ocelot.Apu`

- Register I/O: `readReg :: ApuReg -> ApuState -> Word8`, `writeReg :: ApuReg -> Word8 -> ApuState -> ApuState`
- Time advance: `advance :: MCycles -> ApuState -> (ApuState, [Sample])`

### `Ocelot.Timer`

- Register I/O: `readReg :: TimerReg -> TimerState -> Word8`, `writeReg :: TimerReg -> Word8 -> TimerState -> TimerState`
- Time advance: `advance :: MCycles -> TimerState -> (TimerState, [Interrupt])`

### `Ocelot.Joypad`

- `setButtons :: ButtonState -> JoypadState -> JoypadState` (frontend pushes input)
- `readP1 :: JoypadState -> Word8`
- `writeP1 :: Word8 -> JoypadState -> (JoypadState, [Interrupt])`

The frontend never touches `JoypadState` fields; it calls `setButtons`.

### `Ocelot.Cartridge`

- `read8 :: Word16 -> Cartridge -> Word8` (covers `0x0000-0x7FFF` and `0xA000-0xBFFF`)
- `write8 :: Word16 -> Word8 -> Cartridge -> Cartridge`
- `loadRom :: ByteString -> Either CartridgeError Cartridge`
- `loadSave :: ByteString -> Cartridge -> Cartridge`, `extractSave :: Cartridge -> Maybe ByteString`

MBC variant selection (no-MBC, MBC1, MBC2, MBC3, MBC5, etc.) is internal to `Ocelot.Cartridge`. The bus sees only `read8` and `write8`.

### `Ocelot.Memory`

WRAM, HRAM, and OAM DMA bookkeeping.

- `readWram` / `writeWram`, `readHram` / `writeHram`
- `startOamDma :: Word8 -> Memory -> Memory`, `tickOamDma :: MCycles -> Memory -> (Memory, [DmaCopy])`

### Interrupt Latching

Subsystems return `[Interrupt]` from their `advance` functions; they do not write to `IF` directly. The bus (or the master step path) latches those
edges into `IF` once per advance:

```haskell
data Interrupt = VBlank | LcdStat | TimerInt | Serial | Joypad

latchInterrupts :: [Interrupt] -> Machine -> Machine
```

This keeps each subsystem unaware of where `IF` lives, and makes the interrupt-arrival order a single auditable code path.

### Encapsulation Rule

A subsystem's `*State` type (e.g. `PpuState`, `ApuState`, `TimerState`) is exported as an opaque type from its module: the data constructor and field
accessors are not part of the public API. Use the listed query and effect functions instead. The current exceptions are:

- `Ocelot.Cpu.Registers.Registers (..)`: a leaf data type with no internal state machine, exported in full because there is nothing to hide and the
  surrounding module enforces the F-register low-nibble invariant through smart accessors.
- A deliberate testing facade (`src/Ocelot/Testing.hs`) that grants lower-level access to test code only. Do not add a "just for now" re-export
  anywhere else.

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
- Progress and completeness changes should update `ROADMAP.md` once it exists.
- If you detect stale docs while changing related code, fix them in the same patch.

## Review Guidelines (P0/P1 Focus)

Review output should be concise and only include critical issues.

- `P0`: must-fix defects (incorrect emulation behavior, severe regression, broken build or test workflow).
- `P1`: high-priority defects (likely timing bug, incorrect subsystem coupling, missing validation for a risky change).

Use this review format:

1. `Severity` (`P0`/`P1`)
2. `File:line`
3. `Issue`
4. `Why it matters`
5. `Minimal fix direction`

Do not include style-only feedback or broad praise.
