# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Cubicle is a formal verification tool (model checker) written in OCaml that verifies safety properties of parameterized systems (systems with an arbitrary number of processes). It combines model checking algorithms with automatic SMT theorem provers and invariant inference.

## Build Commands

```bash
./configure          # Generate Makefile (run once, or after configure.in changes)
make                 # Build native binary (cubicle.opt)
make byte            # Build bytecode version (cubicle.byte)
make clean           # Clean build artifacts
make install         # Install to /usr/local/bin (requires sudo)
```

## Testing

```bash
make test            # Run full test suite (safe + unsafe examples)
make test_good       # Test safe systems only (10 examples)
make test_bad        # Test unsafe systems only (4 examples)
```

Tests use a 30-second timeout and verify output contains "The system is SAFE" or "UNSAFE".

## Running Cubicle

```bash
cubicle file.cub              # Backward reachability (default)
cubicle -brab 2 file.cub      # BRAB algorithm with 2-process forward exploration
cubicle -h                    # Show all options
```

Key options: `-quiet`, `-nocolor`, `-brab <n>`, `-forward-depth <d>`, `-enumerative <n>`, `-solver <alt-ergo|z3>`

## Architecture

### Verification Algorithms (main modules)
- `bwd.ml` - Backward reachability with approximation (classic algorithm)
- `brab.ml` - BRAB: Backward Reachability with Approximation and Backtracking
- `forward.ml` - Symbolic forward exploration with path-based reasoning
- `enumerative.ml` - Enumerative forward exploration (optional Murphi oracle)
i
### SMT Solving (`smt/` directory)
- `alt_ergo.ml` - Built-in Alt-Ergo SMT solver
- `z3wrapper.ml` - Z3 solver wrapper (generated from `_actual` or `_fake` based on configure)
- `smt.ml` - Unified SMT interface

### Core Data Structures
- `types.ml` - Terms, atoms, cubes, literals
- `cube.ml` - Symbolic states (cubes)
- `node.ml` - Search tree nodes with path history
- `cubetrie.ml` - Trie for cube management

### Frontend
- `parser.mly`, `lexer.mll` - Parser/lexer for `.cub` files
- `typing.ml` - Type checking
- `options.ml` - Command-line argument handling

### Utilities (`common/` directory)
- `hashcons.ml` - Hash-consing for term canonicalization
- `hstring.ml` - Interned strings

## Generated Files

These files are generated and should not be edited directly:
- `parser.ml`, `parser.mli` (from parser.mly)
- `lexer.ml` (from lexer.mll)
- `muparser.ml`, `muparser.mli`, `mulexer.ml` (Murphi parser)
- `smt/z3wrapper.ml` (from configure) 
- `version.ml` (from make)

## Dependencies

- OCaml 4.08.0+ (tested with OCaml 5.x)
- Standard libraries: unix, nums
- Optional: Functory (parallel), Z3 (alternative solver)

## Documentation

```bash
make doc             # Generate HTML OCamldoc in doc/ocamldoc/
make archi           # Generate module dependency diagram (PDF)
```
