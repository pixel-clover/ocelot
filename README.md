## Ocelot

[![Tests](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/tests.yml?label=tests&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/tests.yml)
[![Lints](https://img.shields.io/github/actions/workflow/status/pixel-clover/ocelot/lints.yml?label=lints&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/actions/workflows/lints.yml)
[![Code Coverage](https://img.shields.io/codecov/c/github/pixel-clover/ocelot?label=coverage&style=flat&labelColor=282c34&logo=codecov)](https://codecov.io/gh/pixel-clover/ocelot)
[![Docs](https://img.shields.io/badge/docs-latest-007ec6?label=docs&style=flat&labelColor=282c34&logo=readthedocs)](docs)
[![License](https://img.shields.io/badge/license-MIT-007ec6?label=license&style=flat&labelColor=282c34&logo=open-source-initiative)](https://github.com/pixel-clover/ocelot/blob/main/LICENSE)
[![Release](https://img.shields.io/github/release/pixel-clover/ocelot.svg?label=release&style=flat&labelColor=282c34&logo=github)](https://github.com/pixel-clover/ocelot/releases/latest)

Ocelot is a Game Boy (DMG) and Game Boy Color (CGB) emulator written in Haskell.
It is also the author's vehicle for learning idiomatic Haskell, so the codebase favors clear, type-driven designs over clever ones.

The project is in an early bootstrapping stage. The SM83 register file is in place; the CPU decoder, MMU, PPU, APU, and cartridge subsystems are still
to come. See [AGENTS.md](AGENTS.md) for the target architecture and [CONTRIBUTING.md](CONTRIBUTING.md) for the development workflow.

### Getting Started

There are two supported ways to set up the development environment.

#### With Nix (Recommended)

A `flake.nix` provides a dev shell with GHC 9.6.6, Stack, HLS, HLint, and fourmolu pinned to matching versions.

```shell
nix develop
stack build
stack test
```

`stack.yaml` sets `system-ghc: true` and `install-ghc: false`, so Stack uses the GHC from the flake rather than downloading its own.

#### With Apt and Stack

```shell
# Install Stack and development dependencies (for Debian-based systems).
make install-deps

# See all available commands and their descriptions.
make help

# Build, test, lint, format-check.
make build
make test
make lint
make format-check
```

### Running

`stack run -- <path-to-rom>` once ROM loading lands. For now the executable is a stub that prints a usage line.

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

### License

This project is licensed under the MIT License (see [LICENSE](LICENSE)).
