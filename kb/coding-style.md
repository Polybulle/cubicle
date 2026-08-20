# Cubicle OCaml house style

Checked: 2026-08-12 at local `HEAD` `3994e6d3`, with established style checked against `origin/master` at `3f761354`.
Sources: representative handwritten modules and interfaces: `typing.ml/.mli`, `safety.ml/.mli`, `fixpoint.ml/.mli`, `bwd.ml/.mli`, `pre.ml/.mli`, `forward.ml/.mli`, `node.ml/.mli`, `cube.ml/.mli`, `prover.ml/.mli`, `approx.ml/.mli`, `trace.ml/.mli`, `variable.ml/.mli`, `options.ml/.mli`, `types.ml/.mli`, `smt/smt_sig.mli`, `common/heap.mli`, and `Makefile.in`. Generated parser/lexer files, extracted Why3 code, and vendored compatibility code were excluded when inferring conventions.

Scope: this is a descriptive guide to the conventions visible in Cubicle, not a claim that every old line is exemplary or that the project must preserve every inconsistency. New work should normally match the dominant local convention unless there is an explicit decision to modernize a subsystem.

## Error handling and control flow

### Domain failures are exceptions

Cubicle does not use the standard `('a, 'e) result` type in its handwritten production code. Expected failures that must cross a module boundary are normally represented by a domain exception, often with a typed payload exposed in the `.mli`:

- `Typing.Error of error * Util.loc`, with an abstract `error` type and `Typing.report` (`typing.mli:22-28`, `typing.ml:23-116`);
- `Smt.Error of error`, with `report` in the solver signature (`smt/smt_sig.mli:21-31`);
- `Safety.Unsafe of Node.t`, used to stop backward search (`safety.mli:18-25`, `bwd.ml:65-111`);
- `Stats.ReachedLimit`, used to abort a search at the configured bound (`stats.mli:18-19,53-55`);
- `Heap.EmptyHeap` for an invalid queue operation (`common/heap.mli:16-35`).

The command-line boundary catches these exceptions, reports them, and selects an exit status (`main.ml:91-137`). A module introducing a new cross-module failure should therefore normally expose an exception and document when it is raised, rather than introduce `result` only for that module.

### Variants describe normal algorithmic outcomes

When every outcome is an ordinary result rather than an exceptional transfer of control, Cubicle uses a dedicated variant. Examples include `Bwd.result = Safe | Unsafe` (`bwd.mli:43-57`) and `Forward.possible_result = Reach | Spurious | Unreach` (`forward.mli:35-38,55-72`). These are not error containers: callers are expected to inspect every case.

### `option` represents local absence or an inconclusive lookup

Recoverable absence is generally represented by `option`: fixpoint checks return evidence with `Some` and failure to establish a fixpoint with `None` (`fixpoint.mli:19-27,39-52`); candidate selection returns `Node.t option` (`oracle.mli:27-33`); trace replay and cube resolution also use options (`forward.mli:68-72`, `cube.mli:101-102`).

### Exceptions are also used for local early exit

The implementation frequently uses a private exception or `Exit` to escape a fold, iteration, or deeply nested computation, then converts it to an ordinary local result. `Fixpoint.Fixpoint` short-circuits an implication search and is caught as `Some evidence` (`fixpoint.ml:54-120`); `Transaction.array_findi` uses a local `Found` exception (`transaction.ml`); simplification code documents `Exit` as its inconsistency signal (`cube.mli:77-85`). This is established control-flow style, especially in performance-sensitive traversals.

### Distinguish user errors from broken invariants

- Use a typed, located domain exception for invalid source input.
- Use a documented algorithm exception for an expected semantic/search event (`Unsafe`, `Unsat`, `ReachedLimit`).
- Use `assert` or `failwith` only for internal invariants or deliberately unimplemented/impossible paths. The codebase uses both, but they should not replace a source diagnostic. Examples include `Variable.build_subst` asserting equal arity (`variable.ml:68-76`) and `Fixpoint.assert_stable` rejecting an internal misuse (`fixpoint.ml:41-43`).
- `Not_found` is widely used internally for table/list lookup, usually caught close to the lookup. New public APIs should document it if it can escape.

## Modules and interfaces

### `.mli` files define the intended boundary

Most substantive modules have explicit interfaces. Interfaces range from abstract types (`Cubetrie.t`) to private records whose invariants remain visible (`Cube.t`) and exposed variants when callers must branch (`Bwd.result`, `Forward.possible_result`). Prefer the narrowest interface that supports actual callers; keep elaboration intermediates private when they are not shared semantic objects.

Interface comments normally state semantic behavior and exceptional behavior, not implementation mechanics. Examples include the distinct-variable invariant in `cube.mli:21-29`, the meaning of relevant instantiation in `instantiation.mli:23-35`, and exceptions raised by `Safety.check` and cube simplification.

### Functors are normal when there is a real policy parameter

Cubicle uses signatures and functors for replaceable algorithms or data-structure policies:

- `Bwd.Make` is parameterized by a priority queue (`bwd.mli:29-67`);
- `Approx.Make` is parameterized by an oracle (`approx.mli:31-44`);
- `Heap.Make` is parameterized by an ordering (`common/heap.mli:18-35`);
- the SMT implementation is selected through a module signature (`smt/smt.ml:18-25`).

Selected implementations are commonly exposed as `Selected` or `SelectedOracle`. A functor is less justified for a one-off namespace split with no genuine alternative policy.

### Modules commonly read global options

Core modules often `open Options` and select behavior at module initialization or through top-level definitions: backward strategy, SMT solver, trace generator, pre-image mode, and diagnostics all follow this pattern. Passing every option explicitly would not match current house style. A refactor should preserve direct `Options` use unless changing configuration architecture is an explicit, separate goal.

### The build order is explicit

Cubicle uses Autoconf and Make. New modules must be inserted in dependency order in both `CMO` and `FILES` in `Makefile.in`; `.depend` is generated and should not be edited by hand (`Makefile.in:100-130,236-246,297-310`). There is no Dune module discovery.

## Implementation idioms

### Data and traversal style

Lists are the default finite collection. Code commonly uses recursive functions, `List.fold_left`, `List.rev_map`, reverse accumulators, and `List.rev_append` to control allocation and order. Hashtables, sets, tries, arrays, and mutable references are used where indexing, memoization, or search state requires them. Mutation is not avoided categorically; it is usually localized in an environment, cache, queue, statistics module, or closure.

Because list order can influence search scheduling and diagnostics, a refactor should preserve the exact fold direction, reversal, and concatenation pattern unless an order change is intentional and tested.

### Naming

- files/modules: singular domain nouns in lowercase filenames and capitalized module names (`node`/`Node`, `cube`/`Cube`, `pre`/`Pre`);
- values and functions: `snake_case`;
- module signatures: often `S` or a descriptive CamelCase name;
- functor constructors: usually `Make`;
- selected implementations: `Selected`;
- short local aliases (`T`, `F`, `SA`, `HAA`) are common in mathematically dense modules.

The codebase tolerates domain abbreviations and short accumulator names. A refactor should not rename established concepts merely for stylistic uniformity.

### Formatting and comments

The source predates automatic formatting and contains tabs and uneven indentation; `.editorconfig` specifies only an eight-column tab width. Dominant visible conventions include the project license header, `open` declarations near the top, blank lines between top-level definitions, pattern matches with one constructor per line, and section banners for major algorithmic phases. Ocamldoc comments belong mainly in `.mli` files. Implementation comments explain algorithmic intent, heuristics, invariants, or non-obvious scheduling—not every expression.

Do not run broad formatting over touched files. Match the neighboring block and keep diffs local.

### Equality and identity

Use domain equality where provided: `Hstring.equal`/`Hstring.list_mem`, `ArrayAtom.equal`, and module-specific compare functions. Physical equality appears where object identity is part of the representation, for example mapping a `transition_info` back to its compiled `transition` and locating graph nodes in the current transaction code. Such uses should be preserved during extraction unless the representation is deliberately changed and all constructors/callers are audited.

### Assertions and semantic invariants

The implementation uses assertions heavily for cases believed unreachable after parsing, typing, or normalization. New assertions should encode a previously established invariant, not silently turn a user error, timeout, or solver result into an internal crash. Correctness-sensitive comments should state the representation or semantic invariant being relied upon.

## Refactoring guidance for transaction code

The house-style audit changes the proposed transaction refactor in two important ways.

First, a `result`-returning transaction elaborator would be foreign to the surrounding code. Transaction source validation already belongs to the typing pass and uses `Typing.Error`. The least invasive boundary is therefore:

1. leave transaction-specific error constructors, reporting, source-location handling, and call validation in `Typing`;
2. move the elaboration algorithms into a `Transaction` module that assumes a validated `Ast.system`;
3. let the absorbed traversal retain a `Transaction.Cycle` exception and translate it to `Typing.Error` at the existing typing boundary.

Second, Cubicle's use of functors does not require retaining every existing functor. `Graph.Make` has no varying policy in practice: its exposed node, edge, and path types are fixed to transaction AST types, and transaction elaboration is its only consumer. The accepted refactor therefore absorbs its concrete representation, traversal, diagnostics, and cycle exception into `Transaction`, and removes `Graph.Make`, `graph.ml`, and `graph.mli`. This is consistent with the observed style: retain functors where they express a real replaceable policy, but avoid a nominal abstraction with one domain-specific consumer.

A house-style public interface can remain small:

```ocaml
type path

exception Cycle of Hstring.t list

val paths : Ast.system -> Ast.system * path list
val finalize : Ast.transition list -> path list ->
  (Variable.t list * Ast.transaction_path) list
```

`Typing.system` continues to perform mode checks and transaction call validation, calls `Transaction.paths`, translates `Transaction.Cycle involved` with its existing `error` helper, compiles transitions through `Pre.make_tau`, and calls `Transaction.finalize`. The moved private diagnostics retain their direct use of `Options.debug` and `Options.verbose`.

This accepted interface is an engineering decision grounded in the observed style; it is not yet implemented.

## Cautions

The codebase is old and internally varied. “House style” should not be used to justify copying defects, stale comments, duplicate definitions, or unchecked `assert false`. The reliable rule is narrower: preserve established public error contracts and semantic representations, follow the local module’s idioms, avoid introducing a lone modern abstraction, and separate behavior-preserving extraction from modernization.
