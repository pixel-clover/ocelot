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

Two subtests remain. `8-instr_effect` fails subtest 3, "POP rp pattern is wrong", after
subtest 2's INC/DEC pattern started passing: reads need SameBoy's
`GB_trigger_oam_bug_read` secondary/tertiary/quaternary patterns, which are
unimplemented, so a read currently gets the write pattern. `7-timing_effect` does not
settle even at ten times the golden suite's cycle cap. It prints a full OAM dump on
every iteration where corruption occurred, so it does far more work now than when
nothing corrupted, but 10x the budget rules out slowness as the whole story and it
should be treated as an unexplained non-termination rather than a timeout.

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

### A Measured Dead End on the VBlank IF Latch

Worth recording so it is not re-attempted blind. On `misc/ppu/vblank_stat_intr-C.gb` the ROM `HALT`s waiting for VBlank; SameBoy enters the handler
at `cyc=65516` and Ocelot at `cyc=65508`, so Ocelot services it 8 T-cycles early. SameBoy's `display.c` lines 2152-2178 raise it at dot 5 of line 144
(`LY := 144` two dots in, `IF |= 1` three dots after that), against Ocelot's dot 0; the halt loop only samples pending interrupts on M-cycle
boundaries, which turns that 5-dot offset into the observed 8.

Moving the raise to dot 5 on its own is **wrong**: it breaks `acceptance/ppu/vblank_stat_intr-GS`, which passes today, and does not fix the CGB
variant it was aimed at. Five ROMs constrain this timing while passing (`blargg interrupt_time`, `vblank_stat_intr-GS`, `intr_1_2_timing-GS`,
`intr_2_0_timing`, and `stat_irq_blocking`), so the raise dot cannot be moved in isolation.

Two leads that were not followed up:

- SameBoy also delays `LY` itself to dot 2 and the STAT mode bits to dot 5. The STAT half is now modelled (`statVblankModeDelay`), which moved no ROM
  either way; `LY` still flips at dot 0. Delaying the `LY` register alone was tried separately and reverted (it fixed `oam_bug/1-lcd_sync` but broke
  `hblank_ly_scx_timing-GS`), so the remaining question is whether the two have to move together.
- The same SameBoy block raises the *OAM* STAT source on entering VBlank (`display.c:2160` and `:2177`, "Entering VBlank state triggers the OAM
  interrupt"). That quirk is a plausible reading of what the failing `-C` variant is actually testing, and is unrelated to the raise dot.

Instruction-granular sampling is still the remaining limit: the callback fires at instruction starts, so a boundary that falls *inside* an
instruction is only bracketed, not pinpointed. The bracketing above is the way around it.

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

No ROM verdict moved yet, because the LYC window was only one of the offsets. The remaining ones now
have somewhere to go: `LY` written +2 into the line, the per-boundary STAT mode-bit offsets, the
enable-line report at +76, and the per-dot `oam_write_blocked` transitions the OAM bug needs. Each is
an event dot plus a handler, added and measured one at a time.

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

### Open: The STAT Mode-Bit Delay Is Not One Constant

`statModeDelay = 4` is right on balance but is not right at every boundary, and the two ROMs still failing in this family both trip on it.
`ppu/stat_lyc_onoff` and `ppu/intr_2_oam_ok_timing` diverge identically: at equal `pc` and `cyc`, SameBoy reads mode 3 where Ocelot still reads mode 2, so
Ocelot's mode 2 -> 3 report is about a dot late.

Lowering the constant to 3 does not help, it hurts: the first divergence moves from line 78 to line **9** on `stat_lyc_onoff` and from 35 to 9 on
`intr_2_oam_ok_timing`, and at line 9 Ocelot then reports mode 3 where SameBoy reads mode 0. So one boundary wants a shorter delay while another wants the
longer one, which is the same shape as the VBlank case already carved out as `statVblankModeDelay = 5`. The fix is per-boundary offsets read out of
SameBoy's `GB_STAT_update` call sites, not a single tuned number. Both values are now pinned by dot-precise tests, so a change either way will show up.

### Open: The Enable Line Needs Its STAT Report Decoupled From Its Mode 3 Start

This supersedes the section below, which recorded the sweep as showing "no change". That was wrong, and
wrong for a known reason: `make tools` relinks against the *installed* library, so a `sed` on
`src/Ocelot/Ppu.hs` followed by `make tools` traces the **old** code. Always `stack build` first.

Redone properly, `lcdOnPreDrawDots` does move the observable, and the result rules the value out rather
than in:

| `lcdOnPreDrawDots` | enable STAT mode 3 | first divergence on `lcdon_timing-GS` |
|---|---|---|
| 78 (current) | +84 dots (SameBoy: +76) | line 83829 |
| 70 | **+76, exact match** | line **26** (`ly=1` vs `ly=0` at cyc 280) |

So 70 fixes the STAT report and breaks the LY phase far earlier; 78 does the reverse. No ROM verdict
distinguishes them and the acid2 hashes survive both. The two constraints are coupled through one
constant, which is the actual finding: on the enable line, *the dot at which STAT reports mode 3* and
*the dot at which mode 3 actually starts* are not related by `statModeDelay`. `visibleModeBits` derives
the former from the latter, so no single value can satisfy both.

The next step is therefore a separate event for the STAT mode-3 report on the enable line, independent
of the internal mode-3 start, which is what the sub-line event mechanism was built for. Note also the
reference-point caveat when re-measuring: `cycleWrite` ticks the bus and *then* writes, on both sides,
so the dot at which `lcdc` first reads enabled is dot 0 and `+N` really is dot N.

### Superseded: The Enable-Line STAT Mode 3 Report Is 8 Dots Late

Measured, not inferred, and recorded because the plausible-looking fix does **not** work. On `lcdon_timing-GS.gb`, taking the instruction where `lcdc`
first reads enabled as the reference, SameBoy reports STAT mode 3 at **+76** dots and Ocelot at **+84**. Both read mode 0 at +68, which bounds SameBoy's
transition to `(68, 76]` against Ocelot's 83 (its internal mode-3 start of 79 plus `statModeDelay`).

The obvious reading is that `lcdOnPreDrawDots` double-counts the 4-dot STAT delay, since 78 came from SameBoy's STAT-visible dot while it feeds Ocelot's
*internal* start. That reading is not sufficient: setting it to 70, which should land the report at 75, moved the internal start as intended (the
dot-precise unit tests flipped) and left the traced transition at +84, unchanged. So the mapping from that constant to the observed report is not a simple
`start + statModeDelay`, and the mechanism is not yet understood. Reverted rather than guessed at.

Three notes for whoever picks it up. `lcdon_timing-GS` wants the report at or before +76, while blargg `oam_bug/1-lcd_sync` *failed* when this constant
was 76 and passes at 78, so one constant appears unable to satisfy both. SameBoy separates the STAT mode-3 report (its dot 78) from the pixel-fetch start
(`mode_3_start`, its dot 83) and Ocelot collapses the two into one boundary, which is the most likely reason those oracles pull opposite ways. And the
`+N` figures are cycles from the *observation* point rather than PPU dots: the LCDC write lands mid-instruction, leaving an unmeasured offset of up to one
instruction, so do not read `+76` as "dot 76".

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
