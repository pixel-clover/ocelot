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
- `ocelot-trace.hs` and `sameboy-trace.c` — the two halves of the SameBoy differential tracer. See "Differential tracing" below.

### Differential tracing against SameBoy

`ocelot-trace` and `sameboy-trace` emit the same per-instruction line format, so a `diff` locates the exact instruction at which the two
emulators first disagree. This is the fastest way to chase an accuracy bug that a test ROM reports only as a final pass/fail.

```
pc=XXXX af=XXXX bc=XXXX de=XXXX hl=XXXX sp=XXXX if=XX ie=XX ly=XXX lcdc=XX cyc=XXXXXXXXXX
```

`stat` is the STAT register (`0xFF41`) as the CPU would read it. It was added because the PPU cluster is almost entirely STAT timing and the register was
invisible in the trace: adding it dropped the first divergence on `mem_timing.gb` from line 8147 to line **32**, which is where the 4-dot STAT mode-bit
delay was found. When chasing a PPU failure, diff this column first.

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

### Open: The STAT Mode-Bit Delay Is Not One Constant

`statModeDelay = 4` is right on balance but is not right at every boundary, and the two ROMs still failing in this family both trip on it.
`ppu/stat_lyc_onoff` and `ppu/intr_2_oam_ok_timing` diverge identically: at equal `pc` and `cyc`, SameBoy reads mode 3 where Ocelot still reads mode 2, so
Ocelot's mode 2 -> 3 report is about a dot late.

Lowering the constant to 3 does not help, it hurts: the first divergence moves from line 78 to line **9** on `stat_lyc_onoff` and from 35 to 9 on
`intr_2_oam_ok_timing`, and at line 9 Ocelot then reports mode 3 where SameBoy reads mode 0. So one boundary wants a shorter delay while another wants the
longer one, which is the same shape as the VBlank case already carved out as `statVblankModeDelay = 5`. The fix is per-boundary offsets read out of
SameBoy's `GB_STAT_update` call sites, not a single tuned number. Both values are now pinned by dot-precise tests, so a change either way will show up.

### Open: The Enable-Line STAT Mode 3 Report Is 8 Dots Late

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
