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
- Write correct and complete sentences.
- Avoid made-up words.
- Do not use a colon in place of a verb. Three uses are fine: joining two clauses inside a complete sentence (the replacement the em-dash rule above
  calls for), introducing the gloss of a list item, and introducing an enumeration, whether as a list or inline ("Methods: `add_node`, `add_nodes`,
  ..."). What a colon must not do is turn a sentence into a label and a definition: write "Merges vector search seeds with text search seeds, then
  expands via BFS" rather than "Hybrid retrieval: merges vector search seeds with text search seeds". That shape belongs to a list item, and carrying it
  into prose (a doc comment summary, a paragraph) leaves a fragment where a sentence was required.
- Use participial phrases and abbreviations scarcely.

## Repository Layout

The current tree is small; this layout describes the target structure as the project grows.
Do not invent modules that do not yet exist when answering questions, but do place new modules according to this map.

- `app/Main.hs`: executable entry point. Argument parsing, ROM loading from disk, headless terminal mode, and the SDL frontend dispatch live here.
- `app/Frontend/Sdl.hs`: SDL2-backed frontend with video, audio, and hotkeys (pause, fast-forward, save states with 5 slots, load state, screenshot,
  GIF recording, ROM switching). Includes a startup screen with a native file picker and drag-and-drop fallback for ROM loading when no ROM is
  provided at launch.
- `app-web/Main.hs`: WASM executable entry point. Exposes the emulator to JavaScript via exported WASM functions; compiled with the GHC WASM
  toolchain (`wasm32-wasi-cabal`, `-f -desktop -f wasm-reactor`). **A `foreign export ccall` declaration is not enough to make a symbol callable from
  JavaScript.** The wasm linker exports only the names listed as `-optl-Wl,--export=<name>` under the `wasm-reactor` flag, so a new export needs an
  entry in **both** `package.yaml` and `ocelot.cabal`; the hpack skew described below means `wasm32-wasi-cabal` reads only the latter, so an entry
  added to `package.yaml` alone has no effect at all. Forgetting it fails silently: the code compiles, the deploy succeeds, and the symbol is simply
  absent from `ocelot.wasm`. Check a built artifact with `strings -n 8 dist/web/ocelot.wasm | grep '^ocelot_'`.
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
    - `src/Ocelot/Web.hs`: `WebSession`, the session-level API the WASM host is built on. Everything in `app-web/Main.hs` is a thin `CInt`-and-pointer
      wrapper over this module, so browser-facing behavior belongs here, not there. Also owns the stall watchdog.
    - `src/Ocelot/Testing.hs`: deliberate testing facade for low-level access.
- `test/`: Hspec suite. `Spec.hs` is the `hspec-discover` entry; per-module specs live alongside as `Ocelot/<Module>Spec.hs`. Cross-cutting specs are
  `IntegrationSpec`, `GoldenSpec` (ROM-driven, gated on `OCELOT_GOLDEN=1`), `CgbSpec`, `BootRomSpec`, `SnapshotSpec`, and `WebSpec`.
- `test/testroms/`: third-party test ROMs the regression suite reads at runtime. Nothing here is committed except `README.md`: the
  ROMs are gitignored and fetched with `make test-roms`. Layout:
    - `test/testroms/mooneye/`: prebuilt mooneye-test-suite ROMs from gekkio.fi (`make mooneye-roms`).
    - `test/testroms/dmg-acid2.gb`: Matt Currie's DMG PPU acid2 (`make acid2-roms`).
    - `test/testroms/cgb-acid2.gbc`: Matt Currie's CGB PPU acid2 (`make acid2-roms`).
- `external/`: third-party source trees pulled in as git submodules. Initialize with `git submodule update --init --recursive`.
    - `external/gb-test-roms/`: blargg test ROM collection from `retrio/gb-test-roms`. Load-bearing for the cpu_instrs,
      instr_timing, mem_timing, dmg_sound, cgb_sound, oam_bug, halt_bug, and interrupt_time regression coverage. The `.gb`
      files live in the submodule and are read directly.
- `docs/`: project documentation and image assets. `make docs` runs Haddock and copies the generated HTML into `docs/haskell/`
  (untracked); `stack haddock` itself writes under `.stack-work`.
- `Makefile`: developer workflow entry points. Build and check with `build`, `test`, `lint`, `format`, `format-check`, `coverage`, and `docs`;
  explore with `repl`; fetch ROMs with `test-roms` (`mooneye-roms`, `acid2-roms`); build diagnostics with `tools` (`sameboy-trace`, `sameboy-core`);
  build the browser bundle with `web-build`; build and serve the container image with `docker-build` and `docker-run`. `lint` and `format` cover
  `src`, `app`, `app-web`, `test`, and `tools`.
- `tools/`: standalone developer diagnostics built by `make tools` into `bin/tools/` (built `-O2 -rtsopts`, so they are usable for
  measurement). `bench.hs` is the throughput benchmark; `ocelot-trace.hs` pairs with `sameboy-trace.c` as a differential tracer against
  SameBoy; `blargg-run.hs` prints a blargg ROM's own serial text and `0xA000` subtest code, which is the cheapest first step on a
  failing blargg ROM; the rest are state-dump probes. See `tools/README.md`.
- `package.yaml`: hpack source of truth. Normally you do not hand-edit `*.cabal`; `stack build` regenerates it. That does not currently hold here:
  `ocelot.cabal` was generated by hpack 0.39.1, which is newer than the hpack bundled with the pinned Stack, so `stack build` prints
  "generated with a newer version of Hpack" and **ignores `package.yaml` entirely**. Until the toolchain catches up, an edit to `package.yaml` has to be
  mirrored into `ocelot.cabal` by hand, and the two kept in agreement. This matters most for the wasm export list, because
  `wasm32-wasi-cabal` reads `ocelot.cabal` and never consults `package.yaml`.
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
- `test/golden-known-failures.txt` is the ratchet for ROM results. A ROM not listed there must pass, and a listed ROM that starts passing also fails
  the run so its line gets deleted. When a change moves a ROM, update that file in the same patch; do not silence a regression by adding an entry
  without saying why in the file. `tests.yml` sets `OCELOT_GOLDEN=1`, so the ratchet runs in CI and not only on a developer's machine; the ROM runs are
  cycle-budgeted rather than wall-clock bounded, which is what makes them deterministic on a shared runner.
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
    - `Apu.advance` (frame sequencer steps on its own 8192-T-cycle `apuFrameTimer`). The period lives in the APU, but the **phase** is owned by the
      bus, because hardware clocks the sequencer off a falling edge of DIV bit 4 and the divider lives in the timer. Two bus entry points supply it:
      `Bus.resetDivider` computes whether zeroing DIV drops the sequencer bit (bit 12 of the internal divider, or bit 13 in double speed so the
      wall-clock rate is unchanged) and hands that edge to `Apu.divReset`; `Bus.writeNr52` calls `Apu.alignFrameTimer` on a power-on transition so the
      next step lands on the next DIV edge rather than a full period later. Both flush the deferred APU time first, since realigning ahead of the
      settle would apply the new phase at the wrong point in the APU's timeline. This is what blargg `dmg_sound`/`cgb_sound`
      `07-len sweep period sync` measures, and it passes; do not "fix" the phase again by giving the APU its own view of DIV
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

Construction and host selection:

- `fromCartridge :: Cartridge -> IO Bus` (picks the host from the cart's CGB flag, `BootPostBoot`)
- `fromCartridgeOnHost :: HostHardware -> BootMode -> Cartridge -> IO Bus` (explicit override)
- `HostHardware = HostDmg | HostCgb` (gates the CGB-only register windows and the PPU render path)
- `BootMode = BootPowerOn | BootPostBoot` (`BootPowerOn` leaves peripherals at hardware reset, which a real boot ROM needs to observe;
  `BootPostBoot` layers in the handoff register values for running a cart directly)
- `installBootRom :: ByteString -> Bus -> IO ()` (maps a boot ROM over `0x0000-0x00FF`, plus `0x0200-0x08FF` on CGB, until the cart writes
  `0xFF50`)

Memory and registers:

- `read8 :: Word16 -> Bus -> IO Word8`
- `write8 :: Word16 -> Word8 -> Bus -> IO ()`
- `advance :: Int -> Bus -> IO ()` (M-cycles; ticks Timer, PPU, OAM DMA, serial transfer, HDMA HBlank step, and the joypad IRQ edge in
  lockstep; the APU is deferred, see below). In CGB double-speed mode the peripherals split two ways, per Pandocs KEY1. The timer/divider,
  serial port, and OAM DMA are clocked from the CPU clock, so they keep their CPU-relative rate and get the unhalved count. The LCD controller,
  all sound timings, and HDMA keep their wall-clock rate and get the halved count (odd M-cycles carry over in `busDoubleSpeedAcc`). Halving the
  timer along with the PPU ran every TAC rate at half speed in double-speed mode and failed blargg `interrupt_time`.
- `takeFrameReady :: Bus -> IO Bool` (one-shot: consumes the "the PPU finished a frame" edge, which is how `runUntilFrame` knows to stop)
- `cpuMCyclesPerLcdFrame :: Bus -> IO Int` (M-cycle budget for one LCD frame at the current speed; frontends and `hang-probe` size a frame with it)
- `isCgb :: Bus -> Bool` and `isDoubleSpeed :: Bus -> IO Bool`

Frontend-facing output. All of these forward straight to the matching `Ocelot.Ppu` or `Ocelot.Apu` function, and exist so a frontend never has to
reach through `busPpu`/`busApu`:

- `framebuffer`, `framebufferRgb`, `framebufferRgbBytes`, `framebufferRgbaBytes`, `framebufferRgbaPtr`, `copyFramebufferRgbWithPitch`,
  `copyFramebufferRgba`
- `drainAudioSamples :: Bus -> IO [Int16]`, `drainAudioSamplesVector :: Bus -> IO (Vector Int16)`, and
  `drainAudioSamplesInto :: Ptr Int16 -> Int -> Bus -> IO Int` (prefer the vector or the into-pointer form in hot paths)
- `drainSerial :: Bus -> IO [Word8]` (takes the bytes the guest has shifted out of the link port, oldest first, and empties the queue; this is the
  verdict channel for the blargg ROMs that declare no cartridge RAM, both in `GoldenSpec` and in `tools/wasm-cpu-check.mjs`)
- `setButton :: Button -> Bool -> Bus -> IO ()` (input entry point; forwards to `Joypad.setButton`)

Called from the CPU, and nowhere else:

- `triggerSpeedSwitch :: Bus -> IO Bool` (the `STOP` handler)
- `resetTimerDiv :: Bus -> IO ()` (also `STOP`; hardware zeroes the divider, which realigns the APU frame sequencer, see Architecture Constraints)
- `takeStallCycles :: Bus -> IO Int` (drains the CPU-stall debit the bus accrued during the current instruction, currently general-mode HDMA;
  the peripherals are already ticked, so the CPU only folds it into `cpuCycles`)
- `triggerOamBug :: Word16 -> Bus -> IO ()` (forwards the DMG OAM-corruption trigger for a 16-bit increment through `0xFE00-0xFEFF`)

APU deferral:

- `flushApu :: Bus -> IO ()` and `discardApuDebt :: Bus -> IO ()` (settle or drop deferred APU time; see below)

The APU is the one subsystem `advance` does not tick in lockstep. It accumulates peripheral M-cycles in `busApuDebt` and settles them in a single
`Apu.advance` at the next point anything can observe APU state. That is sound because the APU raises no interrupt and feeds nothing back into
the bus, so its only observation channels are its own register window (`0xFF10-0xFF3F`, flushed in `read8`/`write8`), the sample drains, and
`Apu.dumpState` (flushed in `Snapshot.save`). It is bit-exact because `Apu.advance` chunks to the next event horizon and so never steps past an
event; `Ocelot.ApuSpec`'s "advance batching equivalence" tests pin that invariant down. `apuDebtHorizon` caps the backlog so a game that never
touches an APU register cannot grow the debt or the sample queue without bound.

**If you add a new way to observe APU state, flush first.**
Reaching `busApu` directly without a `flushApu` reads a stale APU.

Bus is the only place that knows the full address map: it dispatches `0x0000-0x7FFF` and `0xA000-0xBFFF` to the cartridge, the VRAM/OAM windows
to the PPU, the audio register windows to the APU, IO/HRAM/IE to its own buffers, and the CGB extension registers (VBK, BCPS/BCPD, OCPS/OCPD,
WBK, KEY1, HDMA1-5) to the right peer.

### `Ocelot.Cpu`

- `Ocelot.Cpu.Execute.step :: Machine -> IO ()` (one instruction; reads/writes go through `Bus`; cycle accounting is stored on the CPU state)
- `Ocelot.Cpu.Execute.runFor :: Int -> Machine -> IO Int` and `runUntilHalt :: Int -> Machine -> IO Int` (test/headless helpers)
- `Ocelot.Cpu.Execute.runUntilFrame :: Int -> Machine -> IO Int` (runs to the frame-ready edge, with the cycle count as a fallback cap for LCD-off
  periods; this is the per-frame entry point both frontends drive)
- Interrupt servicing is folded into `step`; there is no separately exposed entry point.

CPU never imports `Ocelot.Ppu`, `Ocelot.Apu`, `Ocelot.Timer`, or `Ocelot.Cartridge`. Memory access goes through `Bus`. Beyond `Bus.read8`/`write8`,
the `Ocelot.Bus` import inside `Cpu.Execute` covers exactly five things, and that is the only cross-subsystem coupling outside the bus:
`triggerSpeedSwitch` and `resetTimerDiv` (the `STOP` instruction), `takeStallCycles` (cycle accounting), `takeFrameReady` (`runUntilFrame`), and
`triggerOamBug` (the DMG OAM corruption a 16-bit increment through `0xFE00-0xFEFF` causes, raised from `oamBugOnAddrBus` because the address bus is
the CPU's, not the bus's).
Reading or writing CPU registers from outside `Ocelot.Cpu` is allowed only for tests; production code does not poke `regA`, `regPC`, etc.

### `Ocelot.Ppu`

- Memory windows and registers: `read8 :: Word16 -> PpuState -> IO Word8`, `write8 :: Word16 -> Word8 -> PpuState -> IO ()` (covers VRAM, OAM,
  the LCDC/STAT register surface at `0xFF40-0xFF4B`, plus CGB-only `0xFF4F`, `0xFF68-0xFF6C`)
- Time advance: `advance :: Int -> PpuState -> IO Word8` (returns a flag bitmask: bit 0 = VBlank IRQ, bit 1 = STAT IRQ, bit 2 = HBlank-entered
  for HDMA stepping)
- Framebuffer accessors:
    - `framebuffer :: PpuState -> IO (Vector Word8)` (DMG palette indices)
    - `framebufferRgb :: PpuState -> IO (Vector Word8)` (RGB888 bytes)
    - `copyFramebufferRgb :: Ptr Word8 -> PpuState -> IO ()` (copy into a caller-owned RGB888 staging buffer; preferred for desktop hot paths)
    - `copyFramebufferRgbWithPitch :: Ptr Word8 -> Int -> PpuState -> IO ()` (row-pitched variant for SDL texture uploads)
    - `framebufferRgbBytes :: PpuState -> IO ByteString`
    - `copyFramebufferRgba :: Ptr Word8 -> PpuState -> IO ()` (single `memcpy` from the storable RGBA buffer; used by tests and non-WASM callers)
    - `framebufferRgbaBytes :: PpuState -> IO ByteString`
    - `framebufferRgbaPtr :: PpuState -> Ptr Word8` (stable pointer directly into the RGBA buffer's pinned backing store; valid for the lifetime of
      the `PpuState`; the WASM host uses this to give JS a zero-copy view — no per-frame copy needed)
- CGB hookup: `setCgbMode :: Bool -> PpuState -> IO ()` (called by the bus once at startup)
- CGB render-mode hookup: `setCgbRenderMode :: CgbRenderMode -> PpuState -> IO ()`
- Framebuffer-target hookup: `setFbTarget :: FbTarget -> PpuState -> IO ()` (call once after `initialPpu`, before the first frame; `FbRgb` skips RGBA
  writes on the desktop, `FbRgba` skips RGB writes on the web, `FbBoth` is the default and is used by tests)
- `ppuFbRgba` is backed by `Data.Vector.Storable.Mutable.IOVector`, which allocates a *pinned* buffer (`mallocPlainForeignPtrBytes`) rather than the
  movable GHC-heap unboxed `IOVector` used by all other
  framebuffers. This is what makes `framebufferRgbaPtr` safe to call without a copy: the memory never moves. Do not change this to an unboxed vector.
- STAT write-edge hookup: `takePendingStatIrq :: PpuState -> IO Bool` (called by the bus after PPU register writes that can raise STAT)
- Post-boot LCDC seed: `seedLcdc :: Word8 -> PpuState -> IO ()` (called by the bus for the no-boot-ROM handoff instead of `write8`, because that
  handoff is not a guest-visible LCD enable and must not start the short first scanline)
- Construction and geometry: `initialPpu :: IO PpuState`, `framebufferWidth`/`framebufferHeight :: Int`, and the `PpuMode` constructors
- Bus access gating: `cpuCanReadOam`, `cpuCanWriteOam`, `cpuCanReadVram`, and `cpuCanWriteVram`, each `PpuState -> IO Bool`. The bus consults these
  rather than deriving the window from the mode, because the four blocking windows line up with neither edge of the internal mode; see the
  `lcdon_timing-GS` note in `test/golden-known-failures.txt`.
- DMG OAM bug: `accessedOamRow :: PpuState -> IO Int` (which OAM row the mode-2 scan is on, `-1` outside the window), plus
  `triggerOamBug`, `triggerOamBugBusWrite`, and `triggerOamBugRead`, all `Word16 -> PpuState -> IO ()`. The read and write corruption patterns differ,
  and an access arriving through the bus samples the scan a row later than the CPU's own address bus does; conflating either one is what
  `oam_bug/8-instr_effect` catches.
- Post-load fixup: `resyncMode3End :: PpuState -> IO ()` (`ppuMode3End` is derived state refreshed once per line, so a snapshot load has to
  recompute it or the restored line runs on the previous machine's latch)

`PpuState` exports its field record so `Bus` can route memory accesses and so `Snapshot` can serialize the IORefs and IOVectors directly. Treat
the surface listed above as the contract; do not call other PpuState fields from outside `Ocelot.Ppu` outside Snapshot.

### `Ocelot.Apu`

- Register I/O: `read8 :: Word16 -> ApuState -> IO Word8`, `write8 :: Word16 -> Word8 -> ApuState -> IO ()` (covers `0xFF10-0xFF26` and the wave
  RAM at `0xFF30-0xFF3F`)
- Time advance: `advance :: Int -> ApuState -> IO ()` (queues stereo samples; the bus drains them). Batching must stay bit-exact: `advance n` has
  to produce exactly what `advance 1` repeated @n@ times would, because `Ocelot.Bus` defers APU time and settles it in large chunks. `stepCycles`
  guarantees this by chunking to the next event horizon. Any change to the chunking must keep `Ocelot.ApuSpec`'s batching-equivalence tests green.
- Sample drain: `drainSamples :: ApuState -> IO [Int16]`, `drainSamplesVector :: ApuState -> IO (Vector Int16)`, and
  `drainSamplesInto :: Ptr Int16 -> Int -> ApuState -> IO Int` (same chronological samples; the vector and into-pointer forms exist for frontend hot
  paths)
- Frame-sequencer phase, supplied by the bus because the divider lives in the timer: `divReset :: Bool -> ApuState -> IO ()` (the `Bool` is whether
  zeroing DIV dropped the sequencer bit, so the sequencer clocks once) and `alignFrameTimer :: Int -> ApuState -> IO ()` (set the counter to the
  cycles remaining until the next DIV edge, used on APU power-on). See Architecture Constraints; neither is the APU's own business to work out.
- Construction: `initial :: IO ApuState`
- CGB hookup: `setCgbMode :: Bool -> ApuState -> IO ()`
- Host sample rate: `sampleRate :: Int`
- Snapshot hooks: `dumpState :: ApuState -> IO ByteString`, `loadState :: ByteString -> ApuState -> IO ()`

`ApuState` is exported as an opaque type; the channel and frame-sequencer types stay internal.

### `Ocelot.Timer`

- `TimerState` is exported as a record (DIV, TIMA accumulator, TIMA, TMA, TAC fields are part of the API).
- Pure register I/O: `readDiv`, `readTima`, `readTma`, `readTac`, `writeDiv`, `writeTima`, `writeTma`, `writeTac`.
- Pure time advance: `advance :: Int -> TimerState -> (TimerState, Bool)` (returns `True` when TIMA overflowed at least once).
- Construction: `initialTimer :: TimerState` (a pure value, unlike every other subsystem's `initial`).

The timer is the one peripheral that is still pure. The bus owns the `IORef TimerState` and threads the new state back after each advance.

### `Ocelot.Joypad`

- `setButton :: Button -> Bool -> JoypadState -> IO ()` (frontend pushes input; latches an IRQ edge on a falling-bit transition)
- `readP1 :: JoypadState -> IO Word8`, `writeP1 :: Word8 -> JoypadState -> IO ()`
- `takeIrqPending :: JoypadState -> IO Bool` (consumed by `Bus.advance`)
- `isPressed :: Button -> JoypadState -> IO Bool` and `initial :: IO JoypadState`
- Snapshot hooks: `dumpState :: JoypadState -> IO (Word8, Word8, Bool)`, `loadState :: (Word8, Word8, Bool) -> JoypadState -> IO ()`

`JoypadState` is exported as an opaque type; the frontend never touches its fields.

### `Ocelot.Cartridge`

- `read8 :: Word16 -> Cartridge -> IO Word8` (covers `0x0000-0x7FFF` and `0xA000-0xBFFF`)
- `write8 :: Word16 -> Word8 -> Cartridge -> IO ()`
- `loadRom :: ByteString -> IO (Either CartridgeError Cartridge)`
- `cartridgeHeader :: Cartridge -> Header` (pure; the parsed header is decoded once at load)
- `resetMbc :: Cartridge -> IO ()` (returns the volatile controller registers to power-on while preserving battery-backed RAM and MBC3 RTC
  timekeeping; the SDL frontend's reset hotkey calls this before rebuilding the `Machine`, because a reset is not a cartridge swap)
- Save handling: `loadSave :: ByteString -> Cartridge -> IO ()`, `extractSave :: Cartridge -> IO ByteString`, `cartridgeHasBattery`,
  `extractRam`/`loadRam`
- Snapshot hooks: `dumpMbc :: Cartridge -> IO ByteString`, `loadMbc :: ByteString -> Cartridge -> IO ()` (MBC bank-select state)

MBC variant selection (no-MBC, MBC1, MBC2, MBC3 with RTC, MBC5, and HuC1) is internal. The bus sees only `read8` and `write8`. RTC persistence uses
the
VBA-M-compatible 48-byte suffix appended to the RAM bytes in `extractSave`/`loadSave`.

### `Ocelot.Snapshot`

- `save :: Machine -> IO ByteString` and `load :: ByteString -> Machine -> IO (Either SnapshotError ())`
- Versioned binary format (`OCS1` magic + LE u32 version). Any change to the section layout, including adding a field to a subsystem's
  blob, must bump `currentVersion`; old blobs are then rejected with `UnsupportedVersion`. Adding fields without a bump is what left the
  format silently mutating under version 1 through seven revisions.
- Loading is all-or-nothing: `load` decodes the whole blob into a pure `SnapshotData` through the bounds-checked cursor and only writes
  the machine on a complete decode. A short or corrupt blob returns `TruncatedBlob` with the machine untouched. Do not reintroduce
  "decode straight into the live IORefs" — the register records are lazy in their fields, so a bad read survives as a thunk and detonates
  far from the decode site.
- `Ocelot.Snapshot.Binary` reads are bounds-checked. Use `runCursorChecked` (returns `Nothing` on overrun) for anything parsing untrusted
  bytes; `runCursor` is the lenient zero-filling variant, valid only where an outer decoder already framed the payload length.
- Reaches across subsystems via the per-module `dumpState`/`loadState` hooks listed above and via direct PpuState/Bus field access where the
  state is in IORefs and IOVectors that the per-module hooks would just wrap.

### `Ocelot.Web`

Session-level API for the browser host. `WebSession` is opaque and wraps a `Machine` plus the cartridge, the cached header facts, and the stall
watchdog's state. This is the boundary that keeps browser behavior testable: `app-web/Main.hs` is a `CInt`-and-pointer shim with no logic of its own,
and `test/Ocelot/WebSpec.hs` drives this module directly. New browser-facing behavior goes here, not into the shim.

- Lifecycle: `loadSession :: ByteString -> IO (Either CartridgeError WebSession)` and `runFrame :: WebSession -> IO ()` (one LCD frame)
- Header facts, cached at load so the host does not re-parse: `sessionTitle :: WebSession -> Text`, `sessionHasBattery`, and `sessionIsCgb`
- Input: `setButton :: Button -> Bool -> WebSession -> IO ()`
- Output: `framebufferRgb`, `framebufferRgbBytes`, `framebufferRgbaBytes`, `framebufferRgbaPtr`, `copyFramebufferRgba`, the three
  `drainAudioSamples*` forms, `drainSerialBytes :: WebSession -> IO ByteString`, plus `framebufferWidth`/`framebufferHeight`/`audioSampleRate`
- Persistence: `saveState`, `loadState`, `extractSaveData`, and `loadSaveData`
- One-time hookup: `setFbTargetRgba :: WebSession -> IO ()` (the browser only ever reads RGBA, so skip the RGB writes)

Stall watchdog. `runFrame` fingerprints the finished picture and counts consecutive identical frames.

- `stalledFrames :: WebSession -> IO Int` (a plain counter read, cheap enough to poll every frame; resets to 0 the moment the picture changes, so a
  host that reports on the threshold crossing reports once per stall episode rather than once per frame)
- `stallThreshold :: Int` (180 frames, about three seconds)
- `debugState :: WebSession -> IO ByteString` (`Machine.debugSummary` plus the counter, as text meant to be pasted verbatim into a bug report)

**A still picture is not an error.** Title screens, pause menus, and anything waiting on input hold a frame indefinitely, so crossing the threshold is
a cue to gather diagnostics and never a reason to stop the emulator or show the user an error.

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

Differential tracing: when a ROM fails and the verdict alone does not say why, diff Ocelot against SameBoy instruction by instruction.
`make tools sameboy-trace` builds both halves; `tools/README.md` has the workflow. It needs the `external/SameBoy` submodule. Nothing in
`test/` runs this — it is a manual bisection aid.

Additional validation when relevant:

- `make docs` for Haddock changes on the public API.
- `make coverage` when adding or restructuring tests; check `.stack-work/install/*/hpc/`.
- `make repl` (`stack ghci`) for ad-hoc exploration; do not commit REPL-only helpers.
- `stack run -- <path-to-rom>` for frontend or end-to-end manual checks.

Optimize-mode guidance:

- `-O2` is already in the library's `ghc-options`, so a plain `stack build` is optimized.
- Benchmark with `make tools && bin/tools/bench <rom>`, which drives the SDL frontend's per-frame path and reports the multiple of real
  hardware speed. Take the median of several runs: run-to-run spread is a few percent, comfortably wide enough to hide a small regression.
- Add `+RTS -s` for allocation and GC figures. GC is not currently a factor (productivity sits near 99.5%); allocation *volume* in the
  per-M-cycle path is what costs time, so treat bytes-per-M-cycle as the number to drive down.
- Watch for lazy tuple pattern bindings (`let (a, b) = f x`) in hot paths. They allocate a pair thunk plus a selector thunk per component, and
  the demand analyser often will not fire when the components are consumed several statements later. Prefer `(!a, !b) <- pure (f x)` in `IO`,
  or a strict `case`, so the worker/wrapper can unbox the pair. Removing these from `Bus.advance` and the ALU dispatch cut roughly a fifth of
  total allocation.

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
