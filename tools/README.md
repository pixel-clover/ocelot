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
pc=XXXX af=XXXX bc=XXXX de=XXXX hl=XXXX sp=XXXX if=XX ie=XX ly=XXX lcdc=XX
```

Both start at the cart entry point (`PC=0x100`, post-boot CGB register state).

```
git submodule update --init --recursive     # populates external/SameBoy
make tools sameboy-trace                    # sameboy-trace builds SameBoy's core objects first
bin/tools/ocelot-trace  rom.gb 200000 > /tmp/ocelot.trace
bin/tools/sameboy-trace rom.gb 200000 > /tmp/sameboy.trace
diff -u /tmp/sameboy.trace /tmp/ocelot.trace | head
```

The trace carries `ly` and `lcdc`, so PPU timing drift shows up as well as CPU divergence. It does **not** carry a cycle count, so a
disagreement about *when within an instruction* a bus access lands only surfaces once it has changed an architectural register. That limits
its usefulness for the mooneye `*_timing` ROMs; adding a cycle column would fix that.

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
