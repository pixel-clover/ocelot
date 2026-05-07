<div align="center">
  <picture>
    <img alt="Ocelot Logo" src="logo.svg" height="25%" width="25%">
  </picture>
<br>

<h2>Ocelot</h2>

[![Tests](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/tests.yml?label=tests&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/tests.yml)
[![Lints](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/lints.yml?label=lints&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/lints.yml)
[![Code Coverage](https://img.shields.io/codecov/c/github/pixel-clover/ocelot?label=coverage&style=flat&labelColor=282c34&logo=codecov)](https://codecov.io/gh/pixel-clover/ocelot)
[![License](https://img.shields.io/badge/license-MIT-007ec6?label=license&style=flat&labelColor=282c34&logo=open-source-initiative)](https://github.com/pixel-clover/ocelot/blob/main/LICENSE)
[![Play Online](https://img.shields.io/badge/play%20online-browser-007ec6?style=flat&labelColor=282c34&logo=webassembly)](https://pixel-clover.github.io/ocelot/)
<br>
[![Docker](https://img.shields.io/badge/docker-ghcr.io-007ec6?style=flat&labelColor=282c34&logo=docker)](https://github.com/orgs/pixel-clover/packages/container/package/ocelot-web)
[![Release](https://img.shields.io/github/release/pixel-clover/ocelot.svg?label=release&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/releases/latest)

A Nintendo Game Boy and Game Boy Color emulator in Haskell λ

</div>

---

**Download the latest desktop version of Ocelot from [here](https://github.com/pixel-clover/ocelot/releases)
or [try Ocelot in your web browser](https://pixel-clover.github.io/ocelot/).**

Footage of Ocelot running a few games:

<div align="center">

<table>
  <tr>
    <td align="center" width="33%"><img alt="TLZ demo" src="docs/assets/gif/01_gb_lz.gif" width="100%"><br>The Legend of Zelda: Link's Awakening</td>
    <td align="center" width="33%"><img alt="AI demo" src="docs/assets/gif/03_gb_ai.gif" width="100%"><br>Adventure Island</td>
    <td align="center" width="34%"><img alt="FFA demo" src="docs/assets/gif/04_gb_ffa.gif" width="100%"><br>Final Fantasy Adventure</td>
  </tr>
  <tr>
    <td align="center" width="33%"><img alt="TLZ DX demo" src="docs/assets/gif/02_gbc_lzdx.gif" width="100%"><br>The Legend of Zelda: Link's Awakening DX</td>
    <td align="center" width="33%"><img alt="WL3 demo" src="docs/assets/gif/05_gbc_wl3.gif" width="100%"><br>Wario Land 3</td>
    <td align="center" width="34%"><img alt="SMBDl demo" src="docs/assets/gif/06_gbc_smbd.gif" width="100%"><br>Super Mario Bros. Deluxe</td>
  </tr>
</table>
</div>

### Key Features

- Accurate Game Boy and Game Boy Color emulation
- Very portable; run on Windows, Linux, and macOS, and also in the browser via WebAssembly
- Very configurable, including gameplay input, frontend hotkeys, and rendering settings
- Has a permissive license that allows commercial use

See [ROADMAP.md](ROADMAP.md) for the list of implemented and planned features.

> [!IMPORTANT]
> This project is still in early development, so compatibility is not perfect.
> Bugs and breaking changes are also expected.
> Please use the [issues page](https://github.com/pixel-clover/ocelot/issues) to report bugs or request features.

---

### Quickstart

#### Download the Latest Release

##### A. Desktop

You can download the latest pre-built binaries from the project's [release page](https://github.com/pixel-clover/ocelot/releases).

##### B. Web

You can download and use the latest pre-built Docker image for the web version of Ocelot from the
[GCR](https://github.com/orgs/pixel-clover/packages/container/package/ocelot-web):

```bash
docker run -d -p 8085:80 --rm ghcr.io/pixel-clover/ocelot-web:latest
```

Then open http://localhost:8085 in your browser.

#### Build Ocelot from Source

Alternatively, you can build the emulator from source by following the steps below.

##### 1. Clone the repository

```bash
git clone --depth=1 https://github.com/pixel-clover/ocelot.git
cd ocelot
```

> [!NOTE]
> If you want to run the tests and develop Ocelot further, you need to clone the repository with
> `git clone --recursive https://github.com/pixel-clover/ocelot.git`.
> Test ROMs can then be fetched with `make test-roms`.

##### 2. Build the Ocelot Binary

```bash
# This can take some time
make release
```

If the build is successful, you can find the built binary at `$(stack path --local-install-root)/bin/ocelot`.

#### Run the Emulator

Run the `ocelot` binary to start the emulator GUI:

```bash
ocelot
```

Help menu while the emulator is running:

<div align="center">
<img alt="Ocelot Screenshot" src="docs/assets/img/help_menu_0.1.0.0.png" width="100%">
</div>

Run `ocelot --help` to see the list of available command-line options.

Example output:

```
Ocelot 0.1.0.0 (develop@7bb29) - Game Boy (DMG) and Game Boy Color (CGB) emulator in Haskell

Usage: ocelot [-V|--version] COMMAND

Available options:
  -h,--help                Show this help text
  -V,--version             Print the version and exit

Available commands:
  play                     Run the ROM in the SDL frontend (default mode). Pass
                           --boot-rom to start from a DMG/CGB boot ROM instead
                           of the post-boot register state.
  headless                 Step the CPU for a fixed number of instructions and
                           dump the final state (registers, serial output,
                           disassembly, memory hex dump, VRAM tile preview,
                           framebuffer) to the terminal.
  audio-test               Play a 440 Hz sine tone for 2 seconds via SDL. No ROM
                           needed; verifies the SDL audio path.
  info                     Print the ROM's cartridge header and exit.

SDL key bindings (play): Z=A, X=B, Enter=Start, RShift=Select, Arrows=D-pad,
Space=pause, F1=help overlay, .=frame step, Tab=fast-fwd (held), R=reset,
F5=save state, F6=cycle slot (1-5), F7=load state, F12=screenshot, Escape=quit.
```

---

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to make a contribution.

### License

Ocelot is licensed under the MIT License (see [LICENSE](LICENSE)).

### Acknowledgements

* The logo is made of [image 1](https://www.svgrepo.com/svg/28849/old-game boy-console) and [image 2](https://www.svgrepo.com/svg/373660/haskell).
* This project uses the following resources (for different things like testing, frontend, etc.):
    * [gb-test-roms](https://github.com/retrio/gb-test-roms)
    * [mooneye-test-suite](https://github.com/Gekkio/mooneye-test-suite)
    * [dmg-acid2](https://github.com/mattcurrie/dmg-acid2) and [cgb-acid2](https://github.com/mattcurrie/cgb-acid2)
    * [JetBrains Mono](https://github.com/JetBrains/JetBrainsMono)
    * [SDL](https://github.com/libsdl-org/SDL)

#### Reference Implementations

Ocelot's implementation logic was checked with the following reference material for finding errors and verifying correctness:

* [Pan Docs](https://gbdev.io/pandocs/)
* [SameBoy](https://github.com/LIJI32/SameBoy)
* [Game Boy: Complete Technical Reference](https://gekkio.fi/files/gb-docs/gbctr.pdf)
