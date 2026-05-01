# Ocelot developer tools

Diagnostic utilities for inspecting emulator state while debugging real-game compatibility issues.
Not part of the shipped emulator binary; built only when explicitly requested via `make tools`
(or invoked directly via `stack ghc --no-haddock-deps -- tools/<name>.hs -package ocelot`).

These are headless, deterministic, and operate against the same `Ocelot.Bus`, `Ocelot.Cpu`, and `Ocelot.Ppu` modules the emulator and the test suite
use, so any state divergence they surface is the same divergence production code paths would exhibit.

## Tools

- `diagnose.hs` — runs a ROM for a configurable instruction count and prints CPU/PPU/APU/timer state plus a sample of the framebuffer. Useful first
  probe when a cart shows a white screen or wedges.
- `scan-fb.hs` — runs a ROM and counts non-white pixels in the framebuffer. Reports a few sample non-white pixel coordinates.
- `trace-pc.hs` — periodic-sample histogram of the program counter (CPU instruction pointer) over a window. Bins PCs to find hot loops.
- `dump-vram.hs` — dumps tile data, tilemap, BG attribute bank, and palette RAM in a readable hex format.

## Usage pattern

```
stack ghc --no-haddock-deps -- tools/diagnose.hs -package ocelot -o /tmp/ocelot-diagnose
/tmp/ocelot-diagnose path/to/rom.gb
```

Each tool reads one positional argument (the ROM path).
