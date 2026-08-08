# Cubicle agent guide

## Project

Cubicle is an OCaml model checker for safety properties of array-based parameterized systems. Correctness matters more than code volume or benchmark speed.

Read `README.md` and the relevant interfaces and implementations before changing code. Use papers and formal definitions when a task depends on algorithmic or semantic intent; do not infer that intent from names alone.

## Build and test

Cubicle uses Autoconf and Make, not Dune.

```bash
eval $(opam env)  # Once per session
opam exec -- make
opam exec -- make test
./cubicle.opt <options> <file>
```

- Run `./configure` only if the generated configuration is absent or the task requires reconfiguration.
- `make test` checks known-safe and known-unsafe examples. Report the exact command and result.
- A successful build or test run is regression evidence, not proof that a semantic change is sound.

## Working rules

- Read the relevant `.mli`, type definitions, implementation, and call sites before editing.
- Make the smallest change that addresses the task; avoid unrelated cleanup, reformatting, renaming, dependency upgrades, or build-system changes.
- Edit generator inputs rather than generated files. In particular, edit `.mly`/`.mll` sources rather than generated parser or lexer files. Check `.gitignore` and `Makefile.in` when unsure.
- Treat existing tracked changes and untracked files as user work. Do not clean, restore, overwrite, or add them unless asked.
- Do not commit, push, merge, rebase, or rewrite history unless asked.
- Never read, print, or commit secrets.

## Correctness-sensitive changes

For changes to parsing semantics, symbolic representations, pre-image computation, subsumption, solver interaction, search, or invariant inference:

- state the intended semantic effect and assumptions;
- check a minimal example that distinguishes old and new behavior;
- exercise both safe and unsafe cases when applicable;
- use differential or independent checks when practical;
- compare with the defining paper or formal argument when the change relies on one;
- distinguish soundness, completeness, precision, termination, and performance.

Never treat timeout, crash, parse failure, unsupported input, or solver `unknown` as `safe`, `unsafe`, `sat`, or `unsat`.

## Project notes and memory

`kb/` holds optional, reviewed notes that would otherwise be expensive to reconstruct. Follow `kb/README.md`; do not try to mirror or summarize the whole repository.

Memory may contain short, source-grounded retrieval cues. Include a source path and symbol, paper location, command result, or revision as appropriate. Memory is not evidence: re-open the cited source before relying on a technical claim.
