# Current implementation map: transactions, triggers, and parts

Checked: 2026-08-12 in the working tree based on `e11ecc8`.
Sources: commits `785a2848`, `4dfa1a87`, `8d87011d`, `13987444`, `3117d747`; `parser.mly:238-303`; `ast.mli:49-85,120-174`; `typing.ml:314-512,760-810`; `graph.ml:71-135`; `variable.ml:68-122,227-237,283-285`; `enumerative.ml:810-908,998-1023`; `pre.ml:238-288,343-400`; `node.ml:121-127`; `examples/underscore_*.cub`; `examples/germanish_tx.cub`; `model_tract.cub`; `doc/transaction-paper-outline.md` (design document, not implementation authority).

Scope: this note reports current-source behavior and historical smoke checks. `kb/transactional-mcmt.md` is the primary TxCubicle onboarding reference; `kb/todo.md` contains specified but unfinished transaction work.

## Current language and elaboration

The current syntax has two related constructs:

- `triggered transition t(...)` marks a continuation-only transition.
- A normal transition may end with `triggers t1(args) or ...` or `yields`; omitted `triggers` defaults to yielding.
- A transition body may instead consist of one or more `part name { ... }` blocks. Parts use the same formal parameters and outer guard as their enclosing transition. The formerly separate `transaction` declaration was folded into transition syntax in commit `8d87011d`; the grammar requires a nonempty part list in the braced transaction-body alternative (`parser.mly:273-302`).

`Typing.triggers` validates globally unique transition names, call existence/arity, argument scope, duplicate explicitly supplied actual parameters, and acyclicity of the trigger graph. It extracts all paths beginning at an input (`not triggered`, no parts) and ending at an output (a yielding, non-part transition). `resolve_call` and `path_to_futures` eagerly expand each source path over caller-local underscore equality patterns (`typing.ml:397-472`): an underscore may reuse an already known representative not active in its caller or introduce the next canonical fresh representative; representatives selected by simultaneous underscores are removed from that call's reuse candidates. Named actuals are normalized with the current caller substitution. Thus separate underscore occurrences may alias, while an underscore cannot alias an active caller argument and simultaneous underscores remain distinct.

Each expanded path keeps every distinct representative exactly once in its `globs` list and represents aliasing by reusing that representative in call argument lists. `finalize_future` and `expand_trigger_path` preserve this representation. Forward `Variable.all_permutations` and backward `Variable.permutations_missing` therefore continue to instantiate the unique representatives injectively; no downstream transaction type changed.

### Normalization coverage argument

Assume inductively that `in_scope` is duplicate-free, contains every representative used by the normalized prefix, and that the current caller's normalized arguments are pairwise distinct. For one call, process its optional arguments from left to right. Every concrete underscore value is either equal to a known representative outside the active caller tuple, in which case the corresponding reuse branch selects it, or differs from every known representative, in which case the single fresh branch represents it modulo renaming. Removing a reused representative and advancing past a selected fresh representative enforce same-call distinctness. Appending only selected fresh representatives preserves the `in_scope` invariant. `Variable.build_subst` then preserves each selected identity through later named calls. Induction over path edges gives all and only caller-local admissible equality patterns modulo renaming. Injective instantiation of the resulting unique representatives supplies concrete distinct processes; aliases are already represented syntactically before instantiation.

This is a code-level coverage argument, not a proof of the complete transaction-aware forward or backward algorithms.

`Typing.transaction_paths` removes part-bearing source transitions from the ordinary transition set, creates one primitive transition per part, and enumerates every permutation of a transition's parts. For each permutation it makes a guarded copy of its first part: this copy retains the outer guard and source name; later parts are guard-free and triggered (`typing.ml:314-373`). A trigger path containing a part-bearing transition is expanded by the matching part permutations (`typing.ml:455-481`). The typed system therefore carries:

- `t_trans`: ordinary primitive transitions, plus elaborated transaction-part transitions when transaction processing is enabled;
- `t_transactions`: `(path parameters, future-call path)` values consumed by transaction-aware forward/backward code.

The intended `part` relation is exactly this union of serial permutations: no unrelated transition interleaves between parts. The feature is a candidate for removal.

## Direction-specific options

`-tract` without an explicit argument is normalized to `-tract all`. Explicit values are `none`, `fwd`, `bwd`, `all`, and `ignore` (`options.ml:58-81,189-190`). `-tract fwd` changes only finite enumerative exploration; `-tract bwd` changes only backward pre-image search; `all` changes both. Trigger annotations are accepted and ignored when neither direction is selected. Part-bearing transitions require `-tract all`; the current checker rejects `fwd`, `bwd`, `none`, and `ignore` for them (`typing.ml:740-759`).

## Forward behavior

With `-tract fwd`, `Enumerative.search` compiles `t_transactions`, not `t_trans`. A compiled path executes every component in order with a consistent substitution (`enumerative.ml:810-838`). Only complete path results enter the BFS work queue. Intermediate chain states are recorded in `env.states` for candidate rejection but are not queued (`enumerative.ml:806-908`). Candidate filtering ignores the remaining transaction future of those intermediate states; this is an intentional negative-only heuristic, not boundary-only forward semantics.

## Backward behavior

With `-tract bwd`, `Pre.pre_image` selects `pre_image_path`. A node without a future is duplicated over transaction paths and parameter substitutions. For a node with a future, it pre-images the next path transition without cube normalization, preserves the substitution/path tail in `Node.toward`, and recursively processes intermediate nodes in the same call. `Bwd.search` deliberately skips direct-initial and fixpoint checks for a node that still has a future (`bwd.ml:67-69`). `Cube.elim_ite_simplify_unnorm` is used for intermediate cubes because normalization would invalidate the stored global substitution (`pre.ml:245-252`).

## Executed smoke checks

On 2026-08-12, after integrating caller-local normalization, `make test` passed, including four focused `-tract all -brab 4` regressions. Direct `-tract bwd` and `-tract all` runs classified permitted later aliasing and named propagation as `UNSAFE`, and forbidden caller aliasing and same-call equality as `SAFE`. A direct four-process enumerative debug run (`-tract fwd -only-forward -brab 4`) reached `Bad = True` only for the two intended unsafe models. `./cubicle.opt -nocolor -quiet -tract all examples/germanish_tx.cub` remained `SAFE`.

Commands were run at the checked revision with `timeout 30 ./cubicle.opt -nocolor -quiet`:

| Input and option | Observed result |
|---|---|
| `test_good.cub` | `UNSAFE`, exit 1 |
| `-tract all test_good.cub` | `UNSAFE`, exit 1 |
| `test_bad.cub` | typing error: parts require `-tract all`, exit 2 |
| `-tract all test_bad.cub` | `UNSAFE`, exit 1 |
| `-tract fwd test_good.cub` | `UNSAFE`, exit 1 |
| `-tract bwd test_good.cub` | `UNSAFE`, exit 1 |
| `-tract fwd test_bad.cub` | typing error, exit 2 |
| `-tract bwd test_bad.cub` | typing error, exit 2 |
| `-tract ignore test_bad.cub` | typing error, exit 2 |
| `-tract all examples/germanish_tx.cub` | `SAFE`, exit 0 |

A 90-second BRAB/FLASH attempt on `examples/flash_nodata/tract.cub` under each of `-tract ignore`, `-tract fwd`, and `-tract all` was terminated by the timeout (exit 124). These are inconclusive, not verification results; their partial reports are not benchmark evidence.

## Known limitations and open questions

- Trigger graphs must be acyclic.
- `Approx.select_oracle` rejects `-tract` with `-murphi` (`approx.ml:350-355`). Other reporting/certificate paths have not been audited for transaction awareness.
- `examples/germanish3_tract.cub` failed to parse at line 76 in this checkout under `-tract all`; it needs diagnosis before it is used as a regression.
- `doc/transaction-paper-outline.md` contains intended-semantics and performance claims (including a FLASH table). Treat them as proposals/recorded claims until rerun under documented environment and compared with the source semantics.
