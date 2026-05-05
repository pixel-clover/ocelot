<div align="center">
  <picture>
    <img alt="Ocelot Logo" src="logo.svg" height="25%" width="25%">
  </picture>
<br>

<h2>Ocelot</h2>

[![Tests](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/tests.yml?label=tests&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/tests.yml)
[![Lints](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/lints.yml?label=lints&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/lints.yml)
[![Code Coverage](https://img.shields.io/codecov/c/github/pixel-clover/ocelot?label=coverage&style=flat&labelColor=282c34&logo=codecov)](https://codecov.io/gh/pixel-clover/ocelot)
[![Docs](https://img.shields.io/badge/docs-latest-007ec6?label=docs&style=flat&labelColor=282c34&logo=readthedocs)](docs)
[![License](https://img.shields.io/badge/license-MIT-007ec6?label=license&style=flat&labelColor=282c34&logo=open-source-initiative)](https://github.com/pixel-clover/ocelot/blob/main/LICENSE)
[![Release](https://img.shields.io/github/release/pixel-clover/ocelot.svg?label=release&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/releases/latest)

A Nintendo Game Boy and Game Boy Color emulator in Haskell

</div>

---

Ocelot is a Game Boy (DMG) and Game Boy Color (CGB) emulator written in Haskell.

---

### Quickstart

#### Desktop

```bash
make build
stack run -- play path/to/game.gb
```

#### Web

Ocelot now includes a browser host under `web/`, modeled after the Sandopolis web frontend.
The desktop SDL frontend stays separate; the web version uses a dedicated wasm entrypoint.

Building the web version requires the **GHC wasm toolchain** (`wasm32-wasi-cabal`), not the
regular Stack compiler:

```bash
make web-build
```

That writes the browser-ready files to `dist/web/`, including:

- `index.html`
- `ocelot.js`
- `audio-worklet.js`
- `ocelot.wasm`

Serve `dist/web/` over HTTP with any static file server, then open it in a browser.
The web host supports:

- ROM drag-and-drop and recent-ROM caching
- Canvas video output
- Web Audio playback through an AudioWorklet
- Save states in IndexedDB
- Battery-backed save RAM persistence in IndexedDB

### Documentation

- `app/Frontend/Sdl.hs`: desktop SDL frontend
- `app-web/Main.hs`: wasm entrypoint exports for the browser host
- `src/Ocelot/Web.hs`: browser-friendly emulator session API
- `web/`: browser UI, runtime glue, and audio worklet

---

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to make a contribution.

### License

Ocelot is licensed under the MIT License (see [LICENSE](LICENSE)).

### Acknowledgements

* The logo is from [SVG Repo](https://www.svgrepo.com/svg/28849/old-gameboy-console) with some modifications.
* This project uses material from the following projects and resources (mainly for testing):
    * [gb-test-roms](https://github.com/retrio/gb-test-roms)
    * [mooneye-test-suite](https://github.com/Gekkio/mooneye-test-suite)
    * [dmg-acid2](https://github.com/mattcurrie/dmg-acid2) and [cgb-acid2](https://github.com/mattcurrie/cgb-acid2)

#### Reference Implementations

Ocelot's implementation logic was checked with the following reference material for finding errors and verifying correctness:
* [Pan Docs](https://gbdev.io/pandocs/)
* [SameBoy](https://github.com/LIJI32/SameBoy)
* [Game Boy: Complete Technical Reference](https://gekkio.fi/files/gb-docs/gbctr.pdf)
