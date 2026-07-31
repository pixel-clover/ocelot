## Ocelot developer tools

Diagnostic utilities for inspecting emulator state while debugging real-game compatibility issues.
Not part of the shipped emulator binary; built only when explicitly requested via `make tools`
(or invoked directly via `stack ghc --no-haddock-deps -- tools/<name>.hs -package ocelot`).

These are headless, deterministic, and operate against the same `Ocelot.Bus`, `Ocelot.Cpu`, and `Ocelot.Ppu` modules the emulator and the test suite
use, so any state divergence they surface is the same divergence production code paths would exhibit.

### Tools

- `diagnose.hs` — runs a ROM for a configurable instruction count and prints CPU/PPU/APU/timer state plus a sample of the framebuffer. Useful first
  probe when a cart shows a white screen or wedges.
- `scan-fb.hs` — runs a ROM and counts non-white pixels in the framebuffer. Reports a few sample non-white pixel coordinates.
- `trace-pc.hs` — periodic-sample histogram of the program counter (CPU instruction pointer) over a window. Bins PCs to find hot loops.
- `dump-vram.hs` — dumps tile data, tilemap, BG attribute bank, and palette RAM in a readable hex format.
- `probe-ocps.hs` — dumps CGB OBJ palette RAM through the OCPS/OCPD index register.
- `bench.hs` — throughput benchmark. Drives the SDL frontend's per-frame path and reports the multiple of real hardware speed. See
  "Benchmarking" below.
- `hang-probe.hs` — runs a commercial ROM through the *web* frontend's exact frame loop and reports where it stops making
  progress: any exception, plus the first frame after which the picture went static, with a PC histogram. Takes scripted input
  (`[input-seed]`) and can resume from a save state (`--state FILE`). See "Diagnosing a Game That Freezes" below.
- `blargg-run.hs` — runs a blargg test ROM and prints its serial text and `0xA000` result code verbatim. The golden suite reduces a blargg ROM to
  pass/fail, which discards the subtest number the ROM itself reports. See "Read the ROM's Own Verdict First" below.
- `ocelot-trace.hs` and `sameboy-trace.c` — the two halves of the SameBoy differential tracer. See "Differential tracing" below.

### Differential tracing against SameBoy

`ocelot-trace` and `sameboy-trace` emit the same per-instruction line format, so a `diff` locates the exact instruction at which the two
emulators first disagree. This is the fastest way to chase an accuracy bug that a test ROM reports only as a final pass/fail.

```
pc=XXXX af=XXXX bc=XXXX de=XXXX hl=XXXX sp=XXXX if=XX ie=XX ly=XXX lcdc=XX stat=XX nr52=XX cyc=XXXXXXXXXX
```

`stat` is the STAT register (`0xFF41`) as the CPU would read it. It was added because the PPU cluster is almost entirely STAT timing and the register was
invisible in the trace: adding it dropped the first divergence on `mem_timing.gb` from line 8147 to line **32**, which is where the 4-dot STAT mode-bit
delay was found. When chasing a PPU failure, diff this column first.

`nr52` is the APU control register (`0xFF26`), whose low four bits are the per-channel active flags. It makes length-counter and sweep timing visible,
which is otherwise invisible in a CPU trace: on blargg `07-len sweep period sync` it located the defect as channel 2's length clock landing 8 T-cycles
late (SameBoy clears the flag at `cyc=1760220`, Ocelot at `1760228`), where the register-only trace had only shown a downstream read 1500 instructions
further on.

**Reading it is safe, and that was checked rather than assumed.** SameBoy's `GB_apu_read` calls `GB_apu_run`, a lazy sync, so sampling NR52 every
instruction syncs the *reference* APU far more often than the ROM does — a possible observer effect on the side being treated as ground truth. The check
is cheap: strip the new column from a fresh reference trace and diff it against one captured before the column existed. Identical means no perturbation.
Do this for any new column that touches a lazily-synced subsystem.

`cyc` is CPU-relative T-cycles since the cart entry point, sampled at the start of the instruction on the line. Both sides zero it at hand-off, so
boot-stub accounting cannot offset the column. It is CPU-relative rather than wall-clock on both sides (Ocelot's `cpuCycles`, SameBoy's
`debugger_ticks`), so it keeps ticking at the CPU rate in CGB double-speed mode instead of halving. That pairing holds by construction on both sides
(Ocelot scales only peripherals, in `Bus.advance`; SameBoy increments `debugger_ticks` before its own double-speed shift), but no ROM currently
available under `external/` or `test/testroms/` enters double speed, so it is unverified by measurement.

Both start at the cart entry point (`PC=0x100`) and both pick their hardware model from the cart's CGB flag at header byte `0x143`, so the two halves
run the same hardware model.

The *register* hand-off is not aligned with that model: both sides execute the same boot stub, which leaves the CGB post-boot register set (notably
`A=0x11`) even when the model is DMG. That keeps the two halves identical to each other, which is all a differential diff needs, but it means a ROM
that model-detects by reading `A` will take its CGB path on DMG hardware. Aligning the stub per model would need DMG post-boot register values on both
sides.

`sameboy-trace` takes `--dmg` / `--cgb` to force the model. You need that for any ROM whose host `GoldenSpec.mooneyeHost` overrides: every mooneye ROM
ships with CGB flag `0x00`, so a `-C` test that Ocelot deliberately runs on CGB is otherwise traced against a DMG SameBoy. `misc/ppu/vblank_stat_intr-C`
is exactly that case, and it defaults to `DMG_B` here while Ocelot forces CGB.

**Check the model line before trusting a diff.** `sameboy-trace` prints `model=DMG_B` or `model=CGB_E` to stderr. It used to hardcode `CGB_E` while
`Ocelot.Machine.machineFromCartridgeWithBoot` followed the header, and because every mooneye ROM ships with CGB flag `0x00` (their test code needs no
CGB opcodes), every mooneye trace silently compared DMG-Ocelot against CGB-SameBoy. That is not a small effect: fixing it moved the first divergence on
`acceptance/ppu/stat_lyc_onoff` from line 70 to line 6141, and a PPU change derived from the mismatched evidence turned out to be wrong. If you force a
model on one side, force it on both.

```
git submodule update --init --recursive     # populates external/SameBoy
make tools sameboy-trace                    # sameboy-trace builds SameBoy's core objects first
bin/tools/ocelot-trace  rom.gb 200000 > /tmp/ocelot.trace
bin/tools/sameboy-trace rom.gb 200000 > /tmp/sameboy.trace
diff -u /tmp/sameboy.trace /tmp/ocelot.trace | head
```

The trace carries `ly` and `lcdc`, so PPU timing drift shows up as well as CPU divergence. The `cyc` column separates the two failure modes that a
register-only trace conflates: when the two emulators disagree at the *same* `cyc`, the cycle accounting is fine and a peripheral is out of phase;
when `cyc` itself diverges, the instruction stream consumed different time.

Read the columns in this order:

1. **First non-`ly` divergence.** Strip `ly` before diffing (`sed 's/ ly=[0-9]*//'`); its instruction-granular sampling jitters around line
   boundaries and buries the real first divergence in noise.
2. **Is `cyc` equal on the diverging line?** If yes, the CPU consumed identical time and the divergence is peripheral state. If no, walk back to
   the first line where `cyc` diverges: a control-flow split (a test ROM branching on its own pass/fail byte) also shows up here, so confirm
   whether `pc` diverged first.

A worked example, on `mem_timing.gb`:

```
sameboy: pc=0745 af=0500 ... ly=005 lcdc=91 cyc=0000002108
ocelot : pc=0745 af=0400 ... ly=004 lcdc=91 cyc=0000002108
```

Same `pc`, same `cyc`, and `A` differs only because the preceding `LDH A,(FF44)` read a different `LY`. That rules out cycle accounting and points
at PPU line phase. Bracketing the `LY` transitions against the 456-T-cycle line (each first-occurrence of `LY=n` bounds the true boundary to
`(previous cyc, this cyc]`) put SameBoy's phase in `[176, 180)` and Ocelot's in `[168, 172)`: Ocelot raised `LY`, and the VBlank IF bit with it, 8
T-cycles late for the rest of the run.

That one is now fixed. The cause was the first scanline after the LCD is enabled, which hardware runs short (76-dot mode 2, 448-dot line) and Ocelot
ran at the full 456; see `ppuLcdOnFirstLine` in `src/Ocelot/Ppu.hs`. Both sides now bracket to `[176, 180)`, and the first divergence on
`mem_timing.gb` moved from line 224 to line 8147. The bracketing recipe is worth keeping: it is the way to compare PPU phase through an
instruction-granular trace.

The divergence now first shows up as the VBlank IF bit, at equal `cyc` and equal `LY=144`, with Ocelot latching it one instruction before SameBoy.
That is a separate and finer timing question than the line length, and it is still open. `acceptance/ppu/lcdon_timing-GS` and
`lcdon_write_timing-GS` also still fail; see "Open: The Enable-Line STAT Mode 3 Report Is 8 Dots Late" below for the measurement rather than
duplicating it here.

### Read the ROM's Own Verdict First

Before reaching for the differential tracer, run `blargg-run` and read the number the ROM reports, then read the matching
`.s` file in `external/gb-test-roms/<suite>/source/`. blargg's ROMs `set_test N,"<description>"` before each group, and the
`0xA000` result code is that `N`, so the code names the failing behavior in the ROM author's own words. The source also gives
the expected cycle counts as literal constants.

This is much cheaper than trace bisection, and on `07-len sweep period sync` it was also more accurate. A trace comparison had
pinned that ROM's defect to "channel 2's length clock is 8 T-cycles late", derived from an `nr52` column sampled at instruction
boundaries. That was wrong twice over: the failing subtest was 5, `"Powering up APU MODs next frame time with 8192"`, which is
about channel 1 and about the frame sequencer's *phase*, not a small delay. The 8-cycle figure was the spacing of the polling
loop's own instructions, so it measured the sampling resolution rather than the error, and the sampled divergence was inside a
subtest that passes. The source also shows the real tolerance: the poll loop is 11 M-cycles per iteration and the check accepts
a 5-iteration window, so an error has to exceed ~220 T-cycles to fail at all.

The actual bug was that `Apu.handleNr52` reset `apuFrameStep` to 0 on *any* NR52 bit-7 write. Writing bit 7 to an already-on
APU is not a power-on, and hardware leaves the sequencer's phase alone; resetting it pulled the next length clock a whole step
(8192 T-cycles) early. Subtests 2, 3, and 4 passing was the clue that the period was right and only the power-on phase was
wrong.

A related finding from the same probe: `apuFrameTimer` free-runs in phase with the timer's divider, because both start at
machine init and only a DIV write (`Bus.resetDivider`) can separate them. That makes the realignment in `Bus.writeNr52` a
no-op under current invariants. It is kept as documented defensive code, but it is not what fixed this ROM, and a fix
attributed to it would be misattributed.

To find a phase bug like this, instrument rather than trace: a temporary `Debug.Trace`-style probe printing every frame-sequencer
step and every NR52 write, with a running T-cycle count, showed in one run that the write at `t=1941648` had `wasOn=True` and
still reset the step from 3 to 0. Revert the probe with a file copy, not `git checkout` — `git checkout` on a file with other
uncommitted work discards that work too.

### Two Wave-Channel Constants, and the Limit of M-Cycle Tests

The four blargg wave ROMs (`dmg_sound` 09/10/12 and `cgb_sound` 09) were one diagnosis with two fixes, both read off
`external/SameBoy/Core/apu.c` rather than measured:

- **Trigger delay.** An NR34 trigger delays channel 3's first sample fetch by 6 T-cycles beyond the normal period. SameBoy loads
  `(sample_length ^ 0x7FF) + 3` at `apu.c:2014`; those units are 2 T-cycles each, which you can confirm from the same file without
  external docs: the wave reload is `sample_length ^ 0x7FF` where the square reload is `* 2 + 1`, and the square's period is exactly
  twice the wave's. Ocelot had no delay, so every later sample landed one step early. This alone fixed `cgb_sound` 09 and
  `dmg_sound` 10.
- **The DMG `wave_form_just_read` window.** Ocelot modelled it as 4 T-cycles wide, deliberately, to cover M-cycle read granularity.
  Hardware's is one 2 T-cycle APU step. At the period these ROMs use, `(2048 - 2046) * 2 = 4` T-cycles, a 4-cycle window is open on
  every possible read, so DMG never returned `0xFF` at all. Narrowing it to 2 fixed `dmg_sound` 09 and 12.

`09` is self-oracling and worth knowing about: its wave table is `$00,$11,$22,...,$FF`, one distinct byte per index, so a returned
byte names the index that was read. A probe on `waveRamRead` gives the channel's sample position directly, with no reference
emulator needed.

The unit tests here can only pin these constants to a bucket, and they say so. `Apu.advance` takes M-cycles, so a test read lands
only on a multiple of 4 T-cycles, and fetch times are always even. That makes a trigger delay of 6 indistinguishable from 5, 7, or
8, and a window of 2 indistinguishable from 1. Mutating each constant in *both* directions is what surfaces this: the first pair of
tests written here passed with the delay set to 2 and to 10, and with the window set to 1, which made them nearly worthless as a
guard. Check both directions and record which mutants survive, or the ratchet is the only thing actually holding the behavior.

### The DMG OAM Bug Was a Phase Bug, Not a Sub-Cycle Model

This was written off twice as blocked on a per-T-cycle PPU model. It was not. Wiring
`Ppu.triggerOamBug` into `Bus.read8`/`write8` for `0xFE00-0xFEFF` and into
`Cpu.Execute`'s address-bus instructions, then moving the scan window 4 dots earlier,
took blargg `oam_bug` from 3/8 to 6/8 with no regressions anywhere.

What unblocked it was reading `4-scanline_timing.s`, which states the window in
M-cycles instead of leaving it to be inferred: a trigger at `delay 70224-3` must not
corrupt, `-2` through `-2+18` must, and `+19` must not. That is a 19 M-cycle (76 dot)
window inside an 80-dot mode 2. The old model already had the right *width*, because
`accessedOamRow` returned row 0 for the first 4 dots and `triggerOamBug` ignores rows
below 8; only the phase was wrong, by exactly one M-cycle. `4-scanline_timing` says
which way: it passed "just before" and failed "at first corruption", so the window was
late.

The 76 dots are not a tuned constant. SameBoy's row index is `(index & ~1) * 4 + 8`,
so it starts at row 8 (row 0 is never scanned, and the glitch needs two rows above it)
and reaches 160 in the scan's last 4 dots, past OAM's final row at 152. Those 4 dots
name no row, so nothing there can be corrupted.

The earlier attempt was also judged against a moving target. It regressed
`6-timing_no_bug`, which passes trivially while nothing corrupts, so "wiring costs a
ROM" conflated a real regression with the loss of a test that was passing for the wrong
reason. The enable-line fix that made `1-lcd_sync` pass had also not landed yet, and
that ROM is exactly the one validating LCD-on-to-scanline sync, which this window hangs
off. Re-testing a blocked item after a related fix is cheap; assuming the blocker still
holds is not.

`8-instr_effect` has since passed too, and it needed **two** fixes that had to land
together, which is why either alone looked like it did nothing:

1. Reads take a different corruption pattern from writes. SameBoy's
   `GB_trigger_oam_bug_read` picks between a secondary, three tertiary, and a quaternary
   formula by `accessed_oam_row & 0x18` and, in the `mod 32 == 0` case, by the exact row.
   `Ppu.triggerOamBugRead` transcribes it.
2. An access that reaches OAM *through the bus* samples the scan one row later than the
   CPU's own address bus does, because `cycleRead`/`cycleWrite` tick and then access. So
   `INC/DEC rp` (no bus access) wants the raw row while `POP`, `PUSH`, and `LD A,(HL+/-)`
   want it one row back: `Ppu.accessedOamRowForBusAccess`.

Fault 2 hid fault 1. With the row off by one, the `POP rp` subtest landed on row `0x40`
and so took the quaternary branch where hardware takes the secondary one, so implementing
the patterns changed nothing and looked like a dead end. The way out was to instrument
*both* emulators at the same function and diff the rows: a temporary `fprintf` in
SameBoy's `GB_trigger_oam_bug_read` reported rows 48 and 56 for the two `POP` reads
against Ocelot's 56 and 64, which named the one-row offset immediately. Revert the
submodule edit with `git -C external/SameBoy checkout Core/memory.c` when done.

`7-timing_effect` is the last outstanding ROM and is not a transcription problem.
It does not settle even at a billion instructions, where the golden cap is 80 million, so
treat it as unexplained non-termination rather than a timeout. It sweeps a trigger across
116 timings and prints a full OAM dump on each mismatching one, so it does far more work
when corruption is wrong than when it is absent. **SameBoy does not pass it either**: a
standalone driver over SameBoy's own core on DMG returns `0xd1` where the other seven
oam_bug ROMs return `0x00`. Worth knowing before spending anything more on it, and worth
keeping that driver technique in mind generally — ~60 lines of C against
`external/SameBoy/build/obj/Core/*.o` plus the boot stub from `sameboy-trace.c` answers
"does the oracle even pass this?" directly.

One more repeat of an already-documented trap: `make tools` relinks against the
*installed* library, so a `stack build` alone leaves `bin/tools/*` stale. This bit the
first measurement of this very change, which reported all eight ROMs unmoved.

### Diagnosing a Game That Freezes

`hang-probe` drives a ROM through the same loop the browser does, `runUntilFrame (cpuMCyclesPerLcdFrame + 32)`, while keeping the
`Machine` handle that `Ocelot.Web.WebSession` hides. Use it before reaching for the differential tracer, which needs a target
instruction to bisect towards.

**Scripted input is not optional.** The first version of this tool pressed no buttons, and reported Super Mario Bros. Deluxe,
Wario Land II, and Final Fantasy Adventure as stalled. All three were fine: disassembling the loops showed each sitting in its
normal per-frame VBlank sync (`XOR A; LDH (FF91),A; HALT; poll FF91` in SMB Deluxe, whose handler at `0x0C2D` sets `FF91` at
`0x0CBE`), waiting for a button that never came. A static picture is not a hang. With input, all four CGB titles in `roms/` ran
10800 frames on two seeds with no stall and no exception.

Where a freeze can come from, and what has been ruled out by reading the code:

* **A Haskell exception.** The web build catches these: `ocelot_run_frame` wraps the frame in `try`, records the message in
  `ocelot_last_error`, and returns 0; `ocelot-worker.js` posts `frameError` and sets `running = false`; `ocelot.js` calls
  `showError`. So an exception freezes the picture *permanently* and does show a message. If a freeze has no message, it is not
  this. `hang-probe` reports the same exception directly.
* **The frame loop itself.** Not possible: the cap is a hard bound, there is no early return on `cpuHalted`, and
  `cpuMCyclesPerLcdFrame` doubles in CGB double-speed. A game holding the LCD off just consumes the cap, which the tool counts
  and reports.
* **An unmapped ROM bank.** Both ROM read paths bounds-check and return `0xFF`, so a bad bank cannot throw.
* **The audio queue growing without bound.** `drainSampleQueueInto` calls `clearSampleQueue` unconditionally, so samples that do
  not fit the host buffer are dropped rather than retained. A suspended `AudioContext` costs audio, not memory.

A stale `dist/web/ocelot.wasm` deserves ruling out first of all, since nothing in the repo tracks it and `make web-build` is a
separate step from `make build`. A wasm artifact predating a batch of core fixes behaves exactly like "the web build is worse
than the desktop build".

#### Catching a Runaway Instead of a Stall

A game that takes a wild jump does not stop; it executes data until it happens to loop, and the stall watchdog only notices
about three seconds later, long after the trail has gone cold. `--watch-runaway` steps instruction by instruction and traps on
execution state no working game reaches: the stack pointer inside the ROM region (pushes there hit MBC registers, which is how
a crashed Adventure Island session ended up with spurious bank-high bits), or the program counter in VRAM, absent cartridge
RAM, or the OAM/IO window. WRAM and HRAM are deliberately not trapped, because games legitimately run code from both. The trap
prints the last 48 program counters, which show the routine that walked off.

`--fuzz` replaces the fixed one-button input rota with a seeded random button set per 8-frame slot, biased toward the
hold-Right-and-jump shape of a side-scroller. It also presses D-pad combinations a physical pad cannot produce (Left with
Right, Up with Down), because the web frontend's keyboard input delivers them and games written against real pads have never
been tested with them. Fan seeds out in parallel:

```
for s in $(seq 0 15); do echo $s; done | \
  xargs -P 8 -I{} sh -c 'bin/tools/hang-probe --watch-runaway --fuzz rom.gb 30000 {} > /tmp/probe-{}.log 2>&1'
grep -l TRAP /tmp/probe-*.log
```

#### Turning a Browser Freeze into a Reproduction

Scripted input cannot reach everywhere a person playing well can, so the web frontend captures its own reproduction artifacts.
The Worker keeps a rolling save state, refreshed every ten seconds only while the picture is changing, and records every
button event with the number of frames run. The stall report carries the pre-freeze state, the wedged state, and the input
log since the pre-freeze state. Clicking the Download Report button that appears after a freeze (or running `ocelotStall()`
in the browser console) downloads all of them.

The input log makes the freeze deterministic rather than merely nearby:

```
bin/tools/hang-probe --watch-runaway --state rom-pre-freeze.state --replay rom-input-log.txt rom.gb 2400
```

replays the exact session and the trap prints the program-counter trail of the corrupting routine. Replay fidelity holds
because the browser Worker only applies button changes between frames, which is also when the replay applies them. Without
the log, `--state` plus `--fuzz` searches from seconds before the crash instead of from the title screen, which is the
fallback when the report came from an older build.

### Resolved: The VBlank IF Latch Was the Wrong Lever Entirely

`misc/ppu/vblank_stat_intr-C.gb` now passes, and the dead end recorded here is worth keeping because
the measurement was sound and the conclusion drawn from it was not.

What was measured: SameBoy enters the VBlank handler at `cyc=65516` against Ocelot's `cyc=65508`, so
Ocelot services it 8 T-cycles early, and SameBoy's `display.c` raises `IF |= 1` at dot 5 of line 144
against Ocelot's dot 0. All true. Moving Ocelot's raise to dot 5 then broke
`acceptance/ppu/vblank_stat_intr-GS` and fixed nothing, and five passing ROMs pin that dot
(`blargg interrupt_time`, `vblank_stat_intr-GS`, `intr_1_2_timing-GS`, `intr_2_0_timing`,
`stat_irq_blocking`). The conclusion drawn was that the raise dot could not move, so the ROM was
blocked.

**The raise dot never needed to move.** Reading the ROM source settles it in a couple of minutes:
the test does not measure when VBlank is raised, it measures the *gap* between the VBlank interrupt
and the mode-2 STAT interrupt that entering VBlank also raises. The `-C` and `-GS` variants are the
same test with one number changed — `nops 53/54` against `nops 54/55` — so CGB puts the STAT source
one M-cycle ahead of the VBlank flag and DMG fires them together. Reproducing that means adding an
*earlier STAT edge*, which leaves the VBlank dot those five ROMs constrain exactly where it was. See
`vblankOamStatLeadDots`; four dots earlier is enough, because an M-cycle is all the CPU can resolve.

The lead this section already listed as "not followed up" — "the same SameBoy block raises the *OAM*
STAT source on entering VBlank, and that is a plausible reading of what the `-C` variant is testing"
— was the answer, sitting one paragraph below a conclusion that said the ROM was blocked. Chase the
cheap lead before writing off the ROM.

Still open from the original two leads: SameBoy delays `LY` itself to dot 2 of line 144, where Ocelot
flips it at dot 0. Delaying `LY` alone was tried and reverted (it fixed `oam_bug/1-lcd_sync` but broke
`hblank_ly_scx_timing-GS`), so whether it has to move together with something else is unanswered. No
ROM currently needs it.

Instruction-granular sampling remains the tracer's limit: the callback fires at instruction starts, so
a boundary falling *inside* an instruction is bracketed rather than pinpointed.

### Start With `trace-pc` When a ROM Times Out

A ROM that reports no verdict at all is usually not an accuracy gap; it is a crash. `trace-pc` answers that in one run, and it is much cheaper than a
differential trace. `PC=0x38` at 100% of samples means the CPU is executing `0xFF` and looping on `RST 38h`.

That is how the nine mooneye instruction-timing ROMs were diagnosed. All nine timed out, all nine sat at `PC=0x38`, and the differential trace then
pinned the exact instruction: both emulators reached `pc=FDFE` (echo RAM) at the same `cyc`, where SameBoy read opcode `0xCD` (`CALL`, 24 T-cycles) and
Ocelot read `0xFF` (`RST 38`, 16 T-cycles). The cause was `Bus.addrInDmaUse` locking the CPU off every address below `0xFF00` during OAM DMA, where
hardware only occupies one internal bus. Fixing that passed all nine. None of them was an instruction-timing bug.

Two lessons worth keeping: classify by failure *mode* before reading anything into a failure count, and remember that a register-only trace cannot see
a memory divergence until it corrupts a register. A per-access trace mode would have found this directly.

### The OAM Bug Also Waits on the Line Model

`Ocelot.Ppu.triggerOamBug` and `accessedOamRow` implement SameBoy's write-side
`GB_trigger_oam_bug` and its `accessed_oam_row` walk, with unit tests, but are not called from the bus or
the CPU. That is deliberate, and measured rather than assumed. Triggering it from OAM writes plus the
address-bus instructions (16-bit `INC`/`DEC`, `PUSH`, `POP`, `LD SP,HL`) makes blargg `oam_bug/2-causes`
pass and breaks `6-timing_no_bug`, which checks the timings where the bug must *not* appear:

| Triggers | Result |
|---|---|
| read + write + address bus | +`2-causes`, −`6-timing_no_bug` (net 0) |
| write + address bus | −`6-timing_no_bug` (net −1) |
| read only | no change |

Two things are missing. The trigger window is derived from "mode is 2" rather than SameBoy's per-dot
`oam_write_blocked` transitions, so it over-triggers at the edges; and reads need
`GB_trigger_oam_bug_read`'s separate secondary/tertiary/quaternary patterns, several of them
model-specific, rather than the write pattern. The first of those is the same per-dot problem as below,
so the remaining four `oam_bug` subtests are gated on the line model too.

### Sub-Line Events: The Mechanism Now Exists

The blocker described below is now partly lifted. `stepDots` no longer walks only between mode
boundaries: `boundaryFor` can return a *sub-line event* dot, and `transition` dispatches to a handler
that performs the event and stays in the same mode. `atLycCompareDot` / `lycCompareEvent` are the first
user, stopping the walk at the LYC-compare dot so `statEdge` runs there.

That makes the LYC suppression window expressible, and it is now wired into `computeStatLine` where
last time it could not be: the acid2 hashes are unchanged and no ROM regressed, against the ~80-dot
LYC-interrupt shift the naive version caused. A test pins the stop specifically (disable it and the
STAT IRQ arrives at the mode boundary instead), separately from the tests covering the register view.

The LYC window moved no ROM verdict on its own, being one offset among several. The others have since
been dealt with a different way, and mostly without needing new event dots: the STAT mode-bit lag turned
out to be per-*line* rather than per-boundary (`statModeDelayFor`, zero on the enable line), and the
memory-window edges are read straight off the dot in `withWindowPhase` rather than latched at events.
See "Resolved: Five PPU Timing ROMs" below. What is still outstanding here is `LY` written +2 into the
line, and the per-dot `oam_write_blocked` transitions the OAM bug wants.

### Why the PPU Line Model Has to Change, Not Its Constants

The concrete mechanism, found by trying it. SameBoy's LY=LYC comparison runs off a separate
`ly_for_comparison` that holds -1 at the head of a line and becomes the line number a dot later, so the
value is not simply suppressed on a visible line: it holds the *previous* line number for dots 0-2, is a
no-match for dot 3, and only becomes the current line from dot 4. A VBlank line stores -1 up front and so
is genuinely suppressed from dot 0 to 3, and line 0 stores 0 rather than -1 so it has no no-match dot at
all. An earlier pass here read only the last sleep of the sequence and recorded "1 dot on a visible
line", which was both the wrong length and the wrong shape. Ocelot now models
that for the STAT **register** read (`lycMatches`), which is safe because a register read is a pure
observation.

Wiring the same suppression into `computeStatLine`, which is the obviously "correct" thing, breaks it:
`statEdge` is only called from `transition`, i.e. at mode boundaries. Suppressing the match at dot 0
therefore does not move the LYC rising edge to dot 1 — there is no dot-1 sample — it defers the edge to
the next transition at dot 80, shifting every LYC interrupt by roughly 80 dots. Measured effect: the
`dmg-acid2` framebuffer hash changed, and no ROM verdict improved.

That is the general shape of every remaining failure in this cluster. The offsets are all sub-line, and
a model that samples mode and STAT only at boundaries cannot express them at any constant value. Note
also that `dmg-acid2` and `cgb-acid2` pend through their own `pendingWith` rather than the ratchet, so a
rendering change shows up as a changed hash and **not** as a `REGRESSION` — check them explicitly when
touching interrupt timing.

### Resolved: Five PPU Timing ROMs, and Why Tracing Was the Wrong Tool for Them

Three sections used to sit here: two "Open" write-ups and one "Superseded" one, all built on
differential traces against SameBoy. All five ROMs they covered now pass, and **not one was fixed by
the thing the traces pointed at**. Kept as the standing lesson about when to reach for the tracer.

What the traces said, and what was actually wrong:

| ROM | Trace verdict | Actual cause |
|---|---|---|
| `stat_lyc_onoff` | mode 2 -> 3 STAT report a dot late | Never measures that report. STAT bit 2 is a *latch* the comparison clock drives, and it must freeze while the LCD is off. |
| `intr_2_oam_ok_timing` | same, "diverge identically" | OAM read blocking ended at the internal end of mode 3 instead of at the mode-0 report. |
| `lcdon_timing-GS` | needed `lcdOnPreDrawDots` decoupled from the STAT report | Half right: the enable line carries *no* mode-bit lag. Plus the same OAM/VRAM window bug. |
| `lcdon_write_timing-GS` | not diagnosed | OAM and VRAM writes have their own window edges, one and four dots off the read edges. |
| `intr_2_mode0_timing_sprites` | (never traced) | Object penalties were not modelled at all. |

Why the traces misled. A trace reports the *first* instruction where two emulators disagree on some
sampled column, which is only the bug when the bug is upstream of everything else. For four of these
the first divergence was an unrelated downstream symptom, and "at equal `pc` and `cyc`, SameBoy reads
mode 3 where Ocelot reads mode 2" was a true statement about a dot that no assertion in the ROM ever
looks at. Two whole sections of measurement, including a constant sweep, were spent on it.

**Read the ROM's source first — mooneye's `.s` files state the expected values as literal tables.**
That is what closed all five, and it is cheap. `intr_2_mode0_timing_sprites` carries a table of ~100
object layouts with their expected mode 3 lengths, which is enough to derive the penalty rule outright
(6 dots per object plus one fetch abort per background *tile*, not per object) and to *reject* the
per-object reading of Pandocs' formula, which is off by 45 dots on ten stacked objects.
`lcdon_timing-GS` and `lcdon_write_timing-GS` between them pin all four memory-window edges from 38
expectations. `stat_lyc_onoff` states its whole model in comments.

Two things made the ROM sources directly usable:

- **The ROM records its own results in HRAM.** `lcdon_timing-GS` keeps 24 readings at `$FF80` plus
  `fail_round` / `fail_expect` / `fail_actual` at `$FF98`; `lcdon_write_timing-GS` puts its
  `fail_round` triple at `$FF80`. A throwaway `stack runghc` script that runs the ROM and dumps that
  range names the failing entry exactly, with no tracer and no SameBoy build. Note the two ROMs use
  *different* HRAM layouts, so read each `.ramsection` rather than assuming.
- **The 449-dot enable line is what makes odd dots observable.** DMG's post-enable line is not a
  multiple of 4, so every later line is sampled at dots 3 mod 4 instead of 0 mod 4. That is the only
  reason a one-dot window like "OAM reads stop at dot 3 but writes continue to dot 4" is reachable
  from an M-cycle-granular CPU at all, and it is why these ROMs can pin edges finer than Ocelot's
  4-dot PPU step would suggest.

SameBoy's source stayed valuable throughout; it was the *tracer* that was the wrong tool. Reading
`display.c` dot by dot is what supplied the four window edges and the enable-line `STAT |= 3` dot once
the ROM had said which dot to go look at.

Still true, and still the reason to be careful here: `dmg-acid2` and `cgb-acid2` pend through their
own `pendingWith` rather than the ratchet, so a rendering change shows up as a changed hash and **not**
as a `REGRESSION`. Check them explicitly when touching PPU timing.

### Resolved: LY Boundaries on `mem_timing.gb`

Kept as a worked example of the metric that found it. `mem_timing.gb` used to disagree on `ly` at **81 of the first 8146 sampled instructions** (lines 1
through 8146, the region before the then-first divergence at line 8147) while
matching on every other field, with identical `pc` and `cyc` throughout. Identical cycles mean identical PPU tick counts, so some line boundaries
genuinely landed at different dots while others matched exactly, and a fixed 456-dot line cannot produce an intermittent offset. The suspect was the LCD
being toggled off and on during the test, each re-enable restarting the short first line.

That was right. The enable line's pre-drawing window was 76 dots where hardware uses 78 (79 on DMG), and the line was 448 where DMG uses 449. Correcting
both took the count to **0 of 8146**, and fixed blargg `oam_bug/1-lcd_sync`.

Two notes on method. Count `ly` mismatches against a matched-model trace; do **not** use the phase-bracket figure quoted earlier as a regression
detector, because which dot a boundary is bracketed to depends on where instruction boundaries happen to fall, and it moved by 4 dots during this work
purely from sampling. And strip `ly` when hunting the first divergence, then check `ly` separately: a trace whose *only* disagreement is `ly` reports no
divergence at all under the stripped diff.

This tooling is manual. Nothing in `test/` runs it.

### Benchmarking

```
make tools
bin/tools/bench --frames 600 --runs 5 test/testroms/cgb-acid2.gbc
bin/tools/bench --mode noblit test/testroms/cgb-acid2.gbc   # emulation only
bin/tools/bench test/testroms/dmg-acid2.gb +RTS -s          # allocation and GC
```

Run-to-run spread is a few percent, so compare medians (or the best of several runs, which is more robust to interference from other load).

### Usage pattern

```
stack ghc --no-haddock-deps -- tools/diagnose.hs -package ocelot -o /tmp/ocelot-diagnose
/tmp/ocelot-diagnose path/to/rom.gb
```

Each tool reads one positional argument (which is the ROM path).
