## Haskell Project Template

<div align="center">
  <picture>
    <img alt="Haskell Logo" src="docs/assets/logo/haskell.svg" height="35%" width="35%">
  </picture>
</div>
<br>

[![Tests](https://img.shields.io/github/actions/workflow/status/habedi/template-haskell-project/tests.yml?label=tests&style=flat&labelColor=282c34&logo=github)](https://github.com/habedi/template-haskell-project/actions/workflows/tests.yml)
[![Lints](https://img.shields.io/github/actions/workflow/status/habedi/template-haskell-project/lints.yml?label=lints&style=flat&labelColor=282c34&logo=github)](https://github.com/habedi/template-haskell-project/actions/workflows/lints.yml)
[![Code Coverage](https://img.shields.io/codecov/c/github/habedi/template-haskell-project?label=coverage&style=flat&labelColor=282c34&logo=codecov)](https://codecov.io/gh/habedi/template-haskell-project)
[![Docs](https://img.shields.io/badge/docs-latest-007ec6?label=docs&style=flat&labelColor=282c34&logo=readthedocs)](docs)
[![License](https://img.shields.io/badge/license-MIT-007ec6?label=license&style=flat&labelColor=282c34&logo=open-source-initiative)](https://github.com/habedi/template-haskell-project/blob/main/LICENSE)
[![Release](https://img.shields.io/github/release/habedi/template-haskell-project.svg?label=release&style=flat&labelColor=282c34&logo=github)](https://github.com/habedi/template-haskell-project/releases/latest)

This is a template for Haskell projects.
It provides a minimalistic project structure with pre-configured GitHub Actions, Makefile, and a few useful
configuration files.
I share it here in case it might be useful to others.

### Features

- Minimalistic project structure using Stack and hpack
- Pre-configured GitHub Actions for linting (HLint) and testing (Hspec)
- Makefile for managing the development workflow and tasks like code formatting, testing, linting, etc.
- GitHub badges for tests, code quality and coverage, documentation, etc.
- [Code of Conduct](CODE_OF_CONDUCT.md) and [Contributing Guidelines](CONTRIBUTING.md)

### Getting Started

Check out the [Makefile](Makefile) for available commands to manage the development workflow of the project.

```shell
# Install Stack and development dependencies (for Debian-based systems)
make install-deps
```

```shell
# See all available commands and their descriptions
make help
```

```shell
# Build the project
make build
```

```shell
# Run the application
make run
```

```shell
# Run tests
make test
```

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to make a contribution.

### License

This project is licensed under the MIT License ([LICENSE](LICENSE) or https://opensource.org/licenses/MIT)
