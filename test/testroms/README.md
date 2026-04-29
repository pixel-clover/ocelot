## Test ROMs

This directory includes most test ROMs that the regression suite reads at runtime.
The ROMs themselves are not committed to this repository and are downloaded on demand by `make test-roms`.
Note that the blargg test ROM collection is in `external/gb-test-roms/` as a git submodule and is not included here.

> [!IMPORTANT]
> None of these ROMs are owned by the creator of this project. They are included for educational and emulator-correctness-testing purposes
> only. Please respect the intellectual property rights of the original authors. Any tracked binary in this directory must
> have a license that permits redistribution.

### ROM Files

| Path            | Author            | Source                                           | License | Description                                                                                                                                                                                                                                                                          |
|-----------------|-------------------|--------------------------------------------------|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `dmg-acid2.gb`  | Matt Currie       | https://github.com/mattcurrie/dmg-acid2/releases | MIT     | DMG PPU acid test. Renders a fixed reference image; consumed by the `dmg-acid2` golden case in `test/Ocelot/GoldenSpec.hs`, which hashes the framebuffer.                                                                                                                            |
| `cgb-acid2.gbc` | Matt Currie       | https://github.com/mattcurrie/cgb-acid2/releases | MIT     | CGB PPU acid test. Same idea as dmg-acid2 but for the CGB color rendering pipeline; consumed by the `cgb-acid2` golden case.                                                                                                                                                         |
| `mooneye/`      | Joonas Javanainen | https://gekkio.fi/files/mooneye-test-suite/      | MIT     | The acceptance, emulator-only, manual-only, and madness category mooneye-test-suite ROMs. Acceptance ROMs are auto-discovered and run by the `mooneye-test-suite` group in `GoldenSpec.hs`; failures register as `pendingWith`, not red, while accuracy gaps are still being closed. |

### How to Download

```sh
make test-roms       # Download both mooneye and acid2 ROMs (recommended)
make mooneye-roms    # Mooneye ROMS only
make acid2-roms      # Acid2 ROMs only
```
