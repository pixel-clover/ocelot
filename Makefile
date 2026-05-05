################################################################################
# Configuration and Variables
################################################################################
STACK         ?= stack
JOBS          ?= $(shell nproc || echo 2)
SRC_DIR       := src
APP_DIR       := app
APP_WEB_DIR   := app-web
TEST_DIR      := test
BUILD_DIR     := .stack-work
DOC_OUT       := docs/haskell

################################################################################
# Targets
################################################################################

.PHONY: all build rebuild run test cov lint format format-check doc clean install-deps release help coverage \
 repl setup-hooks test-hooks mooneye-roms acid2-roms test-roms tools sameboy-core sameboy-trace web-build

.DEFAULT_GOAL := help

help: ## Show the help messages for all targets
	@grep -E '^[a-zA-Z0-9_-]+:.*?## ' Makefile | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

all: build test lint doc  ## build, test, lint, and doc

build: ## Build project
	@echo "Building project with $(JOBS) concurrent jobs..."
	$(STACK) build -j$(JOBS)

rebuild: clean build  ## clean and build

run: build  ## Run the main application
	@echo "Running the application..."
	$(STACK) run --

test: ## Run tests
	@echo "Running tests..."
	$(STACK) test -j$(JOBS)

release: ## Build optimized release binary
	@echo "Building the project in Release mode..."
	$(STACK) build --ghc-options="-O2"

clean: ## Remove build artifacts, cache directories, etc.
	@echo "Removing build artifacts, cache, generated docs, etc."
	rm -rf $(BUILD_DIR) $(DOC_OUT)
	$(STACK) clean

lint: ## Run linter checks on Haskell source files
	@echo "Running HLint..."
	$(STACK) exec -- hlint $(SRC_DIR) $(APP_DIR) $(APP_WEB_DIR) $(TEST_DIR)

format: ## Format Haskell source files in-place
	@echo "Formatting Haskell files..."
	$(STACK) exec -- fourmolu -i $(SRC_DIR) $(APP_DIR) $(APP_WEB_DIR) $(TEST_DIR)

format-check: ## Check formatting without modifying files
	@echo "Checking Haskell formatting..."
	$(STACK) exec -- fourmolu --mode check $(SRC_DIR) $(APP_DIR) $(APP_WEB_DIR) $(TEST_DIR)

doc: ## Generate Haddock documentation for the project
	@echo "Generating documentation to $(DOC_OUT)..."
	$(STACK) haddock --no-haddock-deps
	@mkdir -p $(DOC_OUT)
	@echo "Documentation generated. Check .stack-work for output."

install-deps: ## Install system dependencies (for Debian-based systems)
	@echo "Installing system dependencies..."
	sudo apt install haskell-stack libsdl2-dev pkg-config
	$(STACK) setup
	$(STACK) install hlint fourmolu

coverage: ## Generate code coverage report
	@echo "Running tests with coverage enabled..."
	$(STACK) test --coverage
	@echo "Coverage report generated. Check .stack-work/install/*/hpc/"

repl: ## Start GHCi with project loaded
	@echo "Starting GHCi..."
	$(STACK) ghci

setup-hooks: ## Install Git hooks (pre-commit and pre-push)
	@echo "Installing Git hooks..."
	@pre-commit install --hook-type pre-commit
	@pre-commit install --hook-type pre-push
	@pre-commit install-hooks

test-hooks: ## Run Git hooks on all files manually
	@echo "Running Git hooks..."
	@pre-commit run --all-files

# Default mooneye prebuilt ZIP. Override on the command line if a newer one is published, like:
#   make mooneye-roms MOONEYE_URL=https://gekkio.fi/files/mooneye-test-suite/mts-YYYYMMDD-HHMM-XXXXXXX/mts-YYYYMMDD-HHMM-XXXXXXX.zip
MOONEYE_URL ?= https://gekkio.fi/files/mooneye-test-suite/mts-20240127-1204-74ae166/mts-20240127-1204-74ae166.zip
MOONEYE_DIR := test/testroms/mooneye

mooneye-roms: ## Fetch the prebuilt mooneye-test-suite ROMs into `test/testroms/mooneye`
	@if [ -d $(MOONEYE_DIR) ] && [ -n "$$(ls -A $(MOONEYE_DIR) 2>/dev/null)" ]; then \
		echo "ROMs already present at $(MOONEYE_DIR). Delete the directory to re-fetch."; \
		exit 0; \
	fi
	@echo "Fetching mooneye ROMs from $(MOONEYE_URL)"
	@TMP=$$(mktemp -d) && trap "rm -rf $$TMP" EXIT && \
		curl -fL --progress-bar -o $$TMP/mts.zip "$(MOONEYE_URL)" && \
		unzip -q $$TMP/mts.zip -d $$TMP && \
		mkdir -p $(MOONEYE_DIR) && \
		cp -R $$TMP/mts-*/. $(MOONEYE_DIR)/ && \
		echo "Done. ROMs in $(MOONEYE_DIR). Run with OCELOT_GOLDEN=1 to exercise them."

DMG_ACID2_URL ?= https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb
CGB_ACID2_URL ?= https://github.com/mattcurrie/cgb-acid2/releases/download/v1.0/cgb-acid2.gbc
DMG_ACID2_PATH := test/testroms/dmg-acid2.gb
CGB_ACID2_PATH := test/testroms/cgb-acid2.gbc

acid2-roms: ## Fetch the prebuilt dmg-acid2 and cgb-acid2 ROMs into `test/testroms`
	@mkdir -p $(dir $(DMG_ACID2_PATH))
	@if [ ! -f $(DMG_ACID2_PATH) ]; then \
		echo "Fetching dmg-acid2 from $(DMG_ACID2_URL)"; \
		curl -fL --progress-bar -o $(DMG_ACID2_PATH) "$(DMG_ACID2_URL)"; \
	else \
		echo "Already present: $(DMG_ACID2_PATH)"; \
	fi
	@if [ ! -f $(CGB_ACID2_PATH) ]; then \
		echo "Fetching cgb-acid2 from $(CGB_ACID2_URL)"; \
		curl -fL --progress-bar -o $(CGB_ACID2_PATH) "$(CGB_ACID2_URL)"; \
	else \
		echo "Already present: $(CGB_ACID2_PATH)"; \
	fi

test-roms: mooneye-roms acid2-roms ## Fetch all third-party test ROMs into `test/testroms`
	@echo "All test ROMs ready. Run with OCELOT_GOLDEN=1 to exercise them."

TOOLS_OUT := bin/tools
TOOLS_SRCS := $(wildcard tools/*.hs)
TOOLS_BINS := $(patsubst tools/%.hs,$(TOOLS_OUT)/%,$(TOOLS_SRCS))
WEB_OUT := dist/web
WASM_CABAL ?= wasm32-wasi-cabal
WASM_FLAGS := -f -desktop -f wasm-reactor

tools: $(TOOLS_BINS) $(TOOLS_OUT)/sameboy-trace ## Build the developer diagnostic tools under `tools/` into `bin/tools`

$(TOOLS_OUT)/%: tools/%.hs
	@mkdir -p $(TOOLS_OUT)
	@echo "Building $@"
	@$(STACK) ghc --no-haddock-deps -- $< -package ocelot -package containers -o $@ -outputdir $(TOOLS_OUT)/.objs 2>/dev/null

# SameBoy differential trace driver. Reuses the Core/*.o objects that
# `make -C external/SameBoy tester` produces. Flags must match Core's
# build (no GB_DISABLE_* overrides) or struct layouts diverge and the
# binary segfaults on first malloc.
SAMEBOY_DIR    := external/SameBoy
SAMEBOY_OBJS   := $(wildcard $(SAMEBOY_DIR)/build/obj/Core/*.o)
SAMEBOY_CFLAGS := -g -fPIC -std=gnu11 -D_GNU_SOURCE -DGB_VERSION='"local"' -DGB_COPYRIGHT_YEAR='"local"' \
                  -DGB_INTERNAL -D_USE_MATH_DEFINES -I$(SAMEBOY_DIR) -Wno-multichar

sameboy-core: ## Build SameBoy core object files (prerequisite for sameboy-trace)
	@$(MAKE) -C $(SAMEBOY_DIR) tester

$(SAMEBOY_DIR)/build/obj/Core/gb.c.o:
	@$(MAKE) -C $(SAMEBOY_DIR) tester

$(TOOLS_OUT)/sameboy-trace: tools/sameboy-trace.c $(SAMEBOY_DIR)/build/obj/Core/gb.c.o
	@mkdir -p $(TOOLS_OUT)
	@echo "Building $@"
	@gcc $(SAMEBOY_CFLAGS) -c tools/sameboy-trace.c -o $(TOOLS_OUT)/sameboy-trace.o
	@gcc -o $@ $(TOOLS_OUT)/sameboy-trace.o $(SAMEBOY_DIR)/build/obj/Core/*.o -lm -lpthread

sameboy-trace: $(TOOLS_OUT)/sameboy-trace ## Build the SameBoy-side differential trace driver

web-build: ## Build the browser host and wasm module (requires wasm32-wasi-cabal)
	@command -v $(WASM_CABAL) >/dev/null 2>&1 || { \
		echo "Missing $(WASM_CABAL). Install the GHC wasm toolchain first."; \
		exit 1; \
	}
	@echo "Building ocelot-web with $(WASM_CABAL)..."
	@mkdir -p $(WEB_OUT)
	@$(WASM_CABAL) build exe:ocelot-web $(WASM_FLAGS)
	@cp -R web/. $(WEB_OUT)/
	@cp "$$($(WASM_CABAL) list-bin exe:ocelot-web $(WASM_FLAGS))" "$(WEB_OUT)/ocelot.wasm"
	@echo "Web build ready in $(WEB_OUT). Serve that directory over HTTP."
