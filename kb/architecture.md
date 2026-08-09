# Cubicle execution and verification map

Checked: 2026-08-08, worktree `69341da` plus an unrelated pre-existing modification to `examples/germanish.cub`.
Sources: `main.ml:60-137`; `parser.mly:123-303`; `ptree.ml:502-688`; `typing.ml:719-779`; `brab.ml:26-65`; `bwd.ml:45-111`; `pre.ml:313-400`; `safety.mli:18-25`; `fixpoint.mli:19-53`; `approx.ml:231-372`; `enumerative.ml:998-1023,1204-1221`; `Makefile:103-141,383-426`.

Scope: this is the generic/base path. For the transaction-aware overlay, use `kb/transactions.md`; for the intended TxCubicle semantics and the full implementation correspondence, use `kb/transactional-mcmt.md`.

## Authoritative main path

1. `main.ml` parses one `.cub` file with `Parser.system Lexer.token`, then calls `Typing.system`. It delegates the verification run to `Brab.brab` and maps `Bwd.Safe` to exit 0 and `Bwd.Unsafe` to exit 1. Parse/type failures exit 2; limits, solver errors, and uncaught failures exit 1. Do not collapse these outcomes into a safety verdict.
2. `parser.mly` constructs the `Ptree` representation. `Ptree.encode_psystem` normalizes input formulae: initial formulae become universal DNF (`inits_of_formula`), while unsafe formulae and supplied invariants become existential cubes (`unsafes_of_formula`). Guard formulae are split into ordinary conjunctive requirements and `forall_other` universal guards by `guard_of_formula`.
3. `Typing.system` declares the SMT types/symbols, performs syntactic/type checks (unless `-notyping`), optionally initializes subtype analysis, constructs typed transitions with `Pre.make_tau`, and precomputes finite instances of the initial formula (including negated supplied invariants). It returns `Ast.t_system`.
4. `Brab.brab` initializes the selected oracle. In the normal configuration the selected oracle is enumerative only when `-brab n` is active; otherwise it is deliberately grumpy and no approximation is proposed. `search_and_backtrack` restarts if a candidate approximation produces a counterexample, unless the counterexample originates at an original unsafe cube.
5. `Bwd.Selected.search` seeds its priority queue with original unsafe cubes and candidate approximations, and stores supplied invariants in a `Cubetrie`. For a non-future node it first checks direct intersection with an instantiated initial state (`Safety.check`), then performs a fixpoint/coverage test against visited nodes. A non-covered node may be approximated, is pre-imaged, optionally deletes subsumed visited nodes, then is inserted into visited and its predecessors are queued. Empty queues are replenished from postponed predecessors.
6. `Pre.pre_image_normal` computes each predecessor by conjoining a transition guard with the syntactic pullback of every cube atom through the transition's `tr_tau`, normalizing/splitting ITEs, expanding applicable parameter substitutions, adding universal guard instances, and discarding cheaply inconsistent cubes. It returns ordinary and postponed predecessor lists.

## Invariants and finite exploration

- `Approx.approximations` enumerates heuristic strict subcubes of a backward node (subject to literal/process/array heuristics); an approximation is a candidate *only*. Its logical validity is not established by generation.
- With `-brab n`, `Enumerative.init` explicitly explores sizes `n` (or `0..n` with `-upto`). `first_good_candidate` rejects candidates violated by any recorded finite state. A survivor is passed to backward search, which may later reject it through a genuine counterexample and restart.
- The `Oracle.S` contract (`oracle.mli:16-35`) explicitly states that oracle answers do not determine safety/correctness, only BRAB efficiency. A finite-state survivor is therefore evidence for prioritization, not a proof that the candidate is inductive for the parameterized system.

## Representation and proof-direction reminders

`Node.t` wraps a `Cube.t`; a cube contains existential process variables and a conjunction of literals. The checker backward-searches from existential bad cubes. `Node.subset n1 n2` delegates to literal-array subset (`node.ml:129`), so audit the represented-set inclusion and logical implication direction at every pruning/fixpoint change; the names alone are insufficient evidence of soundness.

The configured Make build lists the production module order in `Makefile:103-123`. Parser, lexer, version, and the Z3 wrapper are generated; modify `parser.mly`, `lexer.mll`, or wrapper inputs rather than their generated outputs.

## Open questions

- The exact theorem connecting Cubicle's trie fixpoint test, pre-image normalization, and a reported `SAFE` result still needs a paper/code correspondence note.
- The source tree contains historical integrations (`why/`, `try-cubicle/`, Murphi paths, generated artifacts). This note intentionally does not classify them as live without a call-site-specific investigation.
