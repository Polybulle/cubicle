# Cubicle agent guide

This is the authoritative repository guide for AI coding agents. If another
agent-instruction file conflicts with this one, this file takes precedence.

## Project and priorities

Cubicle is an OCaml model checker for safety properties of array-based
parameterized systems. It combines symbolic backward reachability, SMT solving,
finite forward exploration, and invariant inference.

Correctness and soundness come before code volume, convenience, or benchmark
speed. A successful build or benchmark is regression evidence, not proof that a
semantic change is sound.

Read `README.md`, the relevant interfaces and implementations, and all call
sites before changing code. When behavior depends on an algorithm or semantics,
consult the defining paper or formalization rather than inferring intent from
names alone.

## Build and test

Cubicle uses Autoconf and Make, not Dune. On the maintained macOS setup, use the
Homebrew opam explicitly because `/usr/local/bin/opam` is stale:

```bash
eval "$(/opt/homebrew/bin/opam env)"  # once per shell session
make
make test
./cubicle.opt <options> <file.cub>
```

- Run `./configure` only if generated configuration is absent or the task
  requires reconfiguration.
- Run the narrowest relevant build/check first, then `make test` for production
  changes.
- `make test` checks known-safe and known-unsafe examples. Report the exact
  command and result.
- Use `make depend` when module dependencies or source ordering change.
- Do not classify timeout, crash, parse/type failure, unsupported input, solver
  `unknown`, or resource exhaustion as `SAFE`, `UNSAFE`, `sat`, or `unsat`.

Typical process exit meanings observed in the native entry point are: safe `0`,
unsafe `1`, and parse/type errors `2`. Check the actual command output rather
than relying on the status alone for other failures.

## Working rules

- Inspect the current worktree and branch before relying on previous context.
- Read the relevant `.mli`, shared type declarations, implementation, and call
  sites before editing. Trace symbols to definitions rather than guessing.
- Make the smallest coherent change that addresses the task. Avoid unrelated
  cleanup, formatting, renaming, dependency upgrades, or build-system changes.
- Match the repository's established OCaml style. Do not introduce an isolated
  modern idiom or abstraction without evidence from representative handwritten
  modules.
- Preserve exception-based source diagnostics and the established
  `Typing.Error` boundary unless a semantic redesign is explicitly requested.
- Treat existing tracked modifications and untracked files as user work. Never
  clean, restore, overwrite, stage, or remove them unless asked.
- Do not commit, push, merge, rebase, rewrite history, or discard work unless
  explicitly asked.
- Never read, print, or commit secrets.
- Keep interfaces narrow. Do not generalize a mechanism without a real second
  consumer.

## Correctness-sensitive changes

Parsing semantics, symbolic representations, transaction elaboration,
pre-image computation, subsumption, solver interaction, search scheduling, and
invariant inference require additional discipline:

1. State the intended semantic effect and assumptions.
2. Identify soundness, completeness, precision, termination, and performance
   consequences separately.
3. Add or run a minimal example that distinguishes old and new behavior.
4. Exercise safe and unsafe cases when applicable.
5. Preserve and, where practical, compare diagnostics, source locations,
   traversal order, generated-name order, and exit statuses.
6. Use differential or independent checks when practical.
7. Compare with the defining paper or formal argument when the change relies on
   one, and state any remaining proof obligation.
8. Run the full available regression suite after focused checks.

In search and graph code, list order, fold direction, reversals, mutation timing,
physical identity, and exception propagation may affect observable behavior or
termination. Do not dismiss them as implementation details without checking.

## Source and generated files

The primary verifier source tree is the repository root. Edit generator inputs,
not generated outputs:

- edit `parser.mly`, not `parser.ml` or `parser.mli`;
- edit `lexer.mll`, not `lexer.ml`;
- edit `muparser.mly` and `mulexer.mll`, not generated parser/lexer files;
- treat `version.ml` and `smt/z3wrapper.ml` as generated;
- treat files copied into `try-cubicle/src/` by `make try` as generated copies.

`Makefile.in` is the maintained build input. A generated `Makefile` may need to
be regenerated or kept consistent while testing build changes. Check
`.gitignore` and `Makefile.in` when ownership is unclear.

## Execution map

The native entry point is `main.ml`:

1. `Options` parses the command line and opens the input.
2. `Lexer` and `Parser` produce an `Ast.system`.
3. `Typing.system` validates and compiles it into an `Ast.t_system`.
4. `Brab.brab` coordinates verification and approximation restarts.
5. `Bwd` performs backward search using `Pre.pre_image`.
6. `Safety`, `Fixpoint`, and `Cubetrie` implement initial-state checks,
   coverage/subsumption, and visited-state indexing.
7. `Trace`, `Stats`, and `Dot` produce certificates and diagnostics.

For a maintained, source-grounded call map, read `kb/architecture.md`.

## Module map

### Language, typing, and transactions

- `ast.mli`: parsed and typed shared representations.
- `lexer.mll`, `parser.mly`: `.cub` surface language.
- `ptree.ml`: parse-tree construction and lowering helpers.
- `typing.ml`: source validation, symbol/type checking, transition compilation,
  and typed-system assembly.
- `transaction.ml` / `transaction.mli`: transaction graph representation and
  traversal, cycle detection, trigger-call normalization, `part` elaboration,
  and transaction finalization.

### Verification

- `brab.ml`: backward reachability with approximations and backtracking.
- `bwd.ml`: backward-search engine and queue discipline.
- `pre.ml`: ordinary and transaction-path predecessor computation.
- `safety.ml`: intersection with initial states.
- `fixpoint.ml`, `cubetrie.ml`: coverage, subsumption, and visited-state lookup.
- `approx.ml`: approximation-candidate generation, selection, and invalidation.
- `forward.ml`, `enumerative.ml`, `murphi.ml`: finite forward exploration and
  external Murphi support.

### Symbolic representation and solving

- `types.ml`: terms, atoms, and logical collections.
- `variable.ml`: process variables, substitutions, and permutations.
- `cube.ml`, `node.ml`: symbolic states and search metadata.
- `smt/`: solver abstraction, internal procedures, and optional backends.
- `common/`: hash-consed strings and generic data structures.

### Output and auxiliary trees

- `trace.ml`: proof and certificate output.
- `stats.ml`: counters, limits, and reports.
- `dot.ml`: Graphviz output.
- `try-cubicle/`: browser entry point and web assets.
- `why/`: Why3 models and extracted proof artifacts.

## Transaction support

The command-line option is `-tx` with modes `none`, `fwd`, `bwd`, `all`, and
`ignore`; bare `-tx` means `-tx all`. There is no compatibility alias.

Transaction behavior is correctness-sensitive. Before changing it, read:

- `kb/transactions.md` for the current implementation map and smoke checks;
- `kb/transactional-mcmt.md` for intended semantics, algorithms, and proof
  boundaries;
- `kb/todo.md` for unresolved implementation and semantic work.

Do not infer semantics solely from names such as `trigger`, `part`, `path`, or
`future`. Preserve callee-tuple underscore distinctness, process-identity
propagation, traversal/path ordering, cycle diagnostics, and `part` permutation
behavior unless the requested change explicitly alters them.

For graph traversal, prefer one canonical representation (currently indexed
nodes and successor lists), separate conventional passes for distinct
obligations such as cycle checking and path collection, and conversion back to
domain objects only at reporting/API boundaries.

## Project knowledge and scientific sources

`kb/` contains reviewed notes that are expensive to reconstruct. Follow
`kb/README.md`; do not mirror the repository or add routine session summaries.
Key entries are:

- `kb/architecture.md`: execution, backward search, and BRAB call map;
- `kb/coding-style.md`: observed OCaml conventions and error handling;
- `kb/scientific-foundations.md`: literature and theorem assumptions;
- `kb/transactions.md`: transaction implementation map;
- `kb/transactional-mcmt.md`: primary transaction-semantics onboarding note.

The repository and cited primary literature are authoritative. KB notes and
agent memory are retrieval aids, not evidence: re-open their cited sources
before relying on a technical claim. Mark direct code observations, cited
results, derivations, experiments, hypotheses, and proposals distinctly.

For experiments, record the command, input, revision, environment, exit status,
and relevant output. Treat contradictions as findings to investigate, not as a
reason to silently choose one account.

## Task-oriented starting points

- Syntax or parsing: `parser.mly`, `lexer.mll`, `ptree.ml`, `typing.ml`.
- Type or source diagnostics: `typing.ml` and relevant AST declarations.
- Wrong reachability result: `brab.ml`, `bwd.ml`, `pre.ml`, `safety.ml`.
- Subsumption or pruning: `fixpoint.ml`, `cubetrie.ml`, `approx.ml`.
- Solver behavior: `smt/smt.ml` and the selected backend/procedure.
- Transactions: `transaction.ml`, then callers in `typing.ml`, `pre.ml`, and
  `enumerative.ml`.
- Certificates or reports: `trace.ml`, `stats.ml`, `dot.ml`.
- Browser build: `try-cubicle/main_js.ml` and the `make try` copy rules.

## Completion checklist

Before reporting a code change complete:

- the requested behavior is implemented, not merely planned or stubbed;
- relevant focused checks have run with observed output;
- `make test` has run for production changes, or the omission is explicit;
- `git diff --check` passes;
- the full diff has been reviewed for accidental generated files, unrelated
  edits, stale names, and changes to user work;
- semantic claims are no stronger than the evidence supports.
