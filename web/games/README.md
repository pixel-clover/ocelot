## Bundled Games

Freely licensed Game Boy games shipped with the web build so a visitor can try the emulator without owning a ROM.
`make web-build` copies all of `web/` into `dist/web/`, and the Dockerfile does the same for the container image, so a file placed here is
served automatically.

Only add a game here when its licence grants the right to redistribute it.
Being a free download is not the same thing.
A game that is merely free to obtain, with no licence statement, must not go in this directory, because serving it from GitHub Pages is redistribution.

### Tobu Tobu Girl Deluxe (`tobudx.gb`)

A dual Game Boy and Game Boy Color arcade platformer by Tangram Games, a remaster of
Tobu Tobu Girl.

- Source: <https://github.com/SimonLarsen/tobutobugirl-dx>
- Homepage: <http://tangramgames.dk/tobutobugirldx>
- Code licence: MIT, Copyright (c) Tangram Games. See `LICENSE-tobudx-MIT.txt`.
- Asset licence: images, text, sound, and music are licensed under Creative Commons
  Attribution 4.0 International, <http://creativecommons.org/licenses/by/4.0/>.

The ROM is redistributed unmodified.
Its SHA-256 is `0a0e8018dbbc8d7f8cd99f05e7cdc7b4cc9e358ecfe9377ebfb2291a84c6e310`.

Cartridge configuration, as Ocelot reads it is MBC1, 256 KiB ROM, 8 KiB battery-backed RAM, CGB flag `DmgAndCgb`, SGB flag set.
The web frontend persists the cartridge RAM to IndexedDB, so high scores survive a reload.
