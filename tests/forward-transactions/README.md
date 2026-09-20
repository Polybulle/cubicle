# Cyclic forward exploration (step 10)

## Implemented semantics

With `-tx fwd` or `-tx all`, both forward engines traverse configurations
consisting of program data, a control location, and ordered process bindings.
`cfg.child_calls_of` resolves successor calls over the supplied finite process
domain. It preserves named arguments, enforces distinct callee arguments, and
allows an underscore to reuse a caller argument omitted from the callee tuple.

Initial configurations are neutral. Neutral selects a non-triggered entry without
executing a transition. An internal configuration executes only its named
transition, then follows its declared calls and/or yielding edge. Yielding returns
to neutral, so unrelated entries cannot interleave with an unfinished transaction.
No transaction paths are pre-enumerated. A repeated full configuration is covered
by its earlier visit, not merely by equal program data.

Process identities remain fixed throughout the finite exploration. The symbolic
post-image skips its formula-only renaming. The enumerative transaction branch
does not apply data-only symmetry normalization, even if `forward_sym` is enabled.
This avoids changing data identities without changing the control bindings.
Ordinary forward exploration, including `-tx bwd`, retains the existing algorithms.

The engines still collect intermediate data states for the candidate rejection
heuristic. An internal violation can therefore reject a useful boundary invariant.
Neither rejection nor survival certifies a located invariant; backward verification
remains responsible for that. Local invariants are not introduced by this step.
The stateless symbolic projection uses the same located traversal.

## Bounds

`-forward-depth` counts executable transitions, not neutral edges. Visited tables
retain the smallest discovered depth so a later shorter arrival is not incorrectly
pruned under a depth bound. `-max-forward` bounds processed configurations,
including neutral and internal configurations. Exhausting that budget truncates
the rejection oracle; it is not a safety verdict.

The new configuration count is larger than a count of data states, especially
without symmetry reduction. In the isolated OCaml 4.12.0 build, German at a
10,000-configuration budget stopped with 406 data states and the end-to-end check
hit its 30-second external timeout during backward restarts. Forward-only search
finished quickly. Raising only the forward configuration budget to 100,000 let
exploration finish at 40,419 configurations and 1,497 data states. The parallel
end-to-end check then returned SAFE within the unchanged wall-clock deadline.
The legacy forward branch stored 753 symmetry-reduced data states; those counts
are not directly comparable. No runtime or memory improvement is claimed.

## Reproduce

From the repository root, in its configured OCaml environment:

```sh
make depend
make
make -f Makefile -f tests/forward-transactions/check.mk forward-transactions-check
python3 tests/forward-transactions/run.py
```

For parallel end-to-end tests, build the same sources in an isolated directory
with real Functory and run:

```sh
TEST_CORES=2 python3 tests/forward-transactions/run.py
```

The existing OCaml 4.12.0 switch provided Functory for this run. The main checkout
remained on its existing OCaml 5.4.1 configuration. No package was installed and
no compiler was replaced.

## Tests and observed results

The Python runner generates small models from declarative transition specs and
computes their reachable data projections using an independent explicit-state
interpreter. Its call resolver enumerates full injective tuples and checks named
constraints, rather than using the implementation's recursive underscore filling.
The OCaml probe obtains symbolic states directly and checks enumerative membership
through the public candidate-rejection API. The fixtures and initial states are
process-symmetric, so the oracle's candidate permutations do not alter membership.
It also checks the stateless symbolic traversal's reachable atom set.

The 280 differential cases cover one, two, and three processes; `all`, `fwd`,
`bwd`, and `none` modes; executable depths from zero through three and unbounded;
zero/one configuration budgets; and symmetry-option compatibility. Models cover
forbidden interleaving, unchanged data at different locations, swapped bindings
through a cycle, underscore distinctness, reuse of an omitted caller argument,
no-entry systems, yielding alternatives, and a depth-sensitive join.

All differential cases passed under OCaml 5.4.1 and in the isolated OCaml 4.12.0
build. Six end-to-end BRAB cases passed in the main sequential build and with
real Functory (`-j 2`), including SAFE and UNSAFE cyclic cases and German. The
step-8 and step-9 suites and `make test` also passed in the main build.

Each differential probe has a 10-second external process-group timeout.
End-to-end runs have a 30-second external timeout, 5,000 backward-node limit,
and 100,000 forward-configuration budget. Generated models and actual comparison
results are in `.local/results.json` and `.local/end-to-end.json`; these artifacts
are ignored by Git and regenerated by the runner.

The differential tests validate the tested finite Boolean fragment. They are not
a proof of symbolic post-image correctness, numeric abstraction, or parameterized
verification soundness. Existing unsupported forward-resume entry points and
transaction certificate generation are not changed by step 10.
