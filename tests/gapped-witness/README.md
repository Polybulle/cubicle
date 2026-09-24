# Normalized variables in located nodes

Run from the repository root:

```sh
make
make -f Makefile -f tests/gapped-witness/check.mk gapped-witness-check
python3 tests/gapped-witness/run.py
```

The runner bounds each process group externally and checks verdicts and exit codes.
It requires Python 3 and reuses the existing test runners' timeout helper and finite
transaction interpreter. Generated executables and execution records are in `.local/`.

## Representation contract

For every node constructed by `Node.create`, `cube.vars` contains exactly the
variables in the formula or active location. Both components are renamed by the
same injective substitution to the contiguous prefix `#1` through `#n`. A variable
used only by the location remains in scope. Location argument order, repetitions,
and sharing with data variables are preserved.

`Node.create` computes this scope from the final formula and location. Generic
cube simplifiers are data-only operations; do not use them to replace a located
node's cube without finalizing both components through the constructor again.
Standalone temporary cubes need not satisfy the search-node normalization contract.

Releasing a location recomputes the scope and removes variables no longer used by
the formula. Ordinary backward search creates neutral locations: the transition
arguments recorded in the history are not active control variables. Node dimension
and postponement comparisons count the complete active scope.

The renaming is a bijection between the old and new active variable sets. Applying
it to both components preserves equality, distinctness, and data/control sharing.
The resulting prefix lets the existing allocation helpers and count-indexed solver
formulas operate under their original assumptions. This argument addresses local
renaming, not the separate question of exact small-population semantics.

The normalizer reuses the original cube and location when already normalized.
`Cube.with_vars` changes the variable list without rebuilding the atom set/array.
No solver-side normalization, distinctness reconstruction, or arbitrary-name
freshness scan is added. `variable.ml`, `prover.ml`, `safety.ml`, and `fixpoint.ml`
remain identical to the starting commit in the working tree.

## History and concrete instances

The constructor preserves the supplied pre-normalization history arguments and
ancestor objects. They are not part of the active variable scope. Boundary crossing
retains the original history list, kind, and depth and creates one new search node,
as before. Historical arguments are local names, not a global concrete execution.
The existing replay enumerates transition assignments; it is not redesigned here.

The finite-state candidate checker must inspect `Cube.subst sigma s.cube` before
`Node.create` normalizes the corresponding symbolic node. Checking the normalized
node instead would erase the concrete assignment being tested. This distinction
is the reason for the small change in `Enumerative.alpha_renamings`.

Internal transaction covering remains disabled. A complete scope does not make
atom-only trie lookup suitable for distinguishing control locations or bindings.
The current boundary-only covering and approximation restrictions are unchanged.

## Models and expected results

- `unsafe.cub`: the original reported failure. For any population with at least two
  processes, `start(p); finish(q)` with distinct `p` and `q` reaches the second
  unsafe clause. Expected UNSAFE. The first unsafe clause is unreachable.
- `safe.cub`: no transition sets an A entry true. Expected SAFE. With
  `-nosubtyping`, the baseline incorrectly reports UNSAFE because a remaining
  gapped variable is not aligned with the initialization instance.
- `control-safe.cub`: `start(i,j)` sets A[i], passes the swapped tuple to `carry`,
  then checks A[j] through `finish`. Distinctness makes the transaction unable to
  set Y. Expected SAFE with transaction-aware backward search. With `-tx none`,
  `finish` is independently executable and the expected verdict is UNSAFE.
- `control-unsafe.cub`: the same swapped call chain selects the process whose A
  entry was set. Expected UNSAFE with at least two processes.
- `model.cub`: typing fixture for direct module-level contracts; the transition
  makes the unsafe formula reachable.

Neither experimental `forall_other` nor proc-valued globals in initialization are
used by these regression models. An unconstrained proc-valued global is declared
in the module fixture but does not occur in its initialization.

## Coverage

`check.ml` checks all 256 pairs of subsets of four process variables as data/control
scopes. It checks ordered/repeated control arguments, removal of stale variables,
normalization idempotence and object reuse, a scope extending past #9, prefix-based
fresh allocation, and injectivity/completeness of generated cover substitutions.
It also checks list/trie/naive covering, certificates, cached formula reuse,
control-only distinctness, initialization, history identity, and actual generated
predecessors. It runs on three fixtures in both ordinary and transaction backward
modes. Boundary release is exercised through the production predecessor API.

`run.py` runs 144 end-to-end cases: four models, three transaction modes, six search
strategies, with and without subtyping/deletion. The independent finite checks cover
populations 1 through 5 for the original pair and 1 through 3 for the control pair.
The finite results supplement the direct arguments above; they do not prove the
whole model checker sound.

## Baseline comparison

To compare with a separately built baseline executable:

```sh
python3 tests/gapped-witness/compare.py /absolute/path/to/baseline/cubicle.opt
```

The script alternates baseline/fixed order over five repetitions of seven cases,
records commands, outputs, exit codes, node counts, and wall-clock timings in
`.local/comparison.json`, and bounds each execution externally. The cyclic case
must reach the specified node limit; it is not counted as SAFE or UNSAFE.

Against a fresh build of starting commit `158f0fe`, using OCaml 5.4.1 and the
internal solver, the comparison observed:

| Case | Baseline | Fixed | Median seconds, baseline / fixed |
|---|---|---|---|
| Reported unsafe model | SAFE (wrong) | UNSAFE | 0.0122 / 0.0121 |
| Safe model, no subtyping | UNSAFE (wrong) | SAFE | 0.0120 / 0.0124 |
| Bakery | SAFE | SAFE | 0.0125 / 0.0121 |
| Swimming pool | UNSAFE | UNSAFE | 0.0851 / 0.0853 |
| Control-safe | SAFE | SAFE | 0.0120 / 0.0119 |
| Control-unsafe | UNSAFE | UNSAFE | 0.0119 / 0.0120 |
| Internal cycle, 2000-node bound | LIMIT | LIMIT | 0.0201 / 0.0208 |

These short runs are a performance smoke check, not evidence of unchanged scaling.
Z3 and actual parallel execution were unavailable in this configured build.
The broader existing `make test` suite is also run as a compatibility check;
models using the excluded experimental features are not correctness evidence for
this repair.
