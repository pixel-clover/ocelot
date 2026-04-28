################################################################################
# Configuration and Variables
################################################################################
STACK         ?= stack
JOBS          ?= $(shell nproc || echo 2)
SRC_DIR       := src
APP_DIR       := app
TEST_DIR      := test
BUILD_DIR     := .stack-work
DOC_OUT       := docs/haskell

################################################################################
# Targets
################################################################################

.PHONY: all build rebuild run test cov lint format doc clean install-deps release help coverage repl setup-hooks test-hooks

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
	$(STACK) exec -- hlint $(SRC_DIR) $(APP_DIR) $(TEST_DIR)

format: ## Format Haskell source files in-place
	@echo "Formatting Haskell files..."
	$(STACK) exec -- fourmolu -i $(SRC_DIR) $(APP_DIR) $(TEST_DIR)

format-check: ## Check formatting without modifying files
	@echo "Checking Haskell formatting..."
	$(STACK) exec -- fourmolu --mode check $(SRC_DIR) $(APP_DIR) $(TEST_DIR)

doc: ## Generate documentation using for the project
	@echo "Generating documentation to $(DOC_OUT)..."
	$(STACK) haddock --no-haddock-deps
	@mkdir -p $(DOC_OUT)
	@echo "Documentation generated. Check .stack-work for output."

install-deps: ## Install system dependencies (for Debian-based systems)
	@echo "Installing system dependencies..."
	sudo apt install haskell-stack
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
