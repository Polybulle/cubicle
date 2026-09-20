# Internal covering: disabled pending review

## Current implementation

Search again uses `Cubetrie` for storage and `Fixpoint.FixpointTrie` for checks.
`bwd.ml` contains the boundary policy in both schedulers. Internal obligations are
expanded and counted toward resource limits, but are neither stored as covers nor
subjected to quick covering, SMT fixpoints, or subsumption deletion. Boundary
covering and neutral-only candidate handling remain active.

The fused `Fixpoint.Located` store/checker and `Covers` alias are removed. The
internal normalization, restricted instantiation, subset check, and SMT check are
commented out in `fixpoint.ml` as a checker-only proposal. They are not exported
or executed. Restoring actual internal coverage needs a separate review of the
algorithm and its storage needs; uncommenting that proposal alone does not enable it.

`bwd.ml` is identical to the step-8 version at `7e87574`. Relative to the branch
starting point `12830fc`, its only remaining edits are the two candidate-admission
filters. `fixpoint.mli` is restored to its pre-step-9 interface. No module selection
layer is needed for this experiment because only the existing boundary checker is
active.

## Controlled cycle experiment

The before binaries were built from `1d92c67`, with internal covering enabled.
The after binaries contain the disabled implementation. Each before/after pair
uses the same compiler and scheduler: OCaml 5.4.1 sequentially, and an isolated
OCaml 4.12.0 build with real Functory and `-j 2`.

Each pair uses `-tx bwd -nodes 100`, the same search strategy and postponement
setting, and a 10-second external process-group timeout. The matrix contains
seven models, BFS and DFS, and postponement 0, 1, and 2: 42 pairs per scheduler,
84 pairs overall. All runs completed with a verdict or Cubicle node-limit exit;
none required the external timeout and none crashed.

| Model | Covering enabled | Covering disabled |
|---|---|---|
| `internal-cycle.cub` | SAFE | Node limit in all configurations |
| `swap-safe.cub` | SAFE | Node limit in all configurations |
| `reachable-cycle-safe.cub` | SAFE | SAFE in all configurations |
| `loop-exit-unsafe.cub` | UNSAFE | UNSAFE except sequential DFS with postponement 0/1, which reaches the node limit |
| `unbounded-internal.cub` | Node limit | Node limit |
| `internal-invariant-safe.cub` | SAFE | SAFE |
| `internal-invariant-unsafe.cub` | UNSAFE | UNSAFE |

The reachable safe-cycle model can loop through `enter` and `spin` while Y stays
Idle. Its unsafe condition is Y = Bad. The guard on `spin` blocks that backward
obligation, and the separate `bad` transition requires Y already Bad. Consequently,
its reachable forward loop does not require internal covering to prove safety.

The DFS unsafe regression was also inspected with a small non-quiet node bound:
the history repeatedly follows `spin`, while queued alternatives remain unexplored.
BFS still reaches the entry and finds the counterexample. Parallel scheduling in
the tested implementation also found it for every tested option combination.

These observations confirm loss of convergence when backward exploration needs to
close an internal cycle. They do not establish that every safe cyclic system must
fail, or that unsafe systems are unaffected. A node-limit exit is inconclusive,
not a SAFE or UNSAFE verdict.

## Reproduce

Build the enabled revision separately and retain its executable. For the current
boundary-only contracts and integration checks:

```sh
make
make -f Makefile -f tests/located-covering/check.mk located-covering-check
python3 tests/located-covering/run.py
TEST_CORES=2 python3 tests/located-covering/run.py # real Functory required
```

For the controlled comparison:

```sh
python3 tests/located-covering/compare.py \
  --before tests/located-covering/.local/covering-enabled.opt \
  --output tests/located-covering/.local/disabled-sequential.json
```

For parallel comparison, supply the enabled and disabled Functory binaries through
`--before` and `--after`, and add `--cores 2`. The report contains executable hashes,
commands, exit statuses, classifications, and output. The saved comparison binaries
and reports are local ignored artifacts, not committed dependencies.

## Regression coverage

The current unit probe checks that identical internal obligations, different
locations, and different bindings are all expanded rather than covered. A matching
supplied invariant must not cover them, and the ordinary boundary fixpoint check
must still work. Integration cases exercise the intentional node limits and the
remaining SAFE/UNSAFE outcomes under all postponement settings.

The current covering tests and step-8 suite passed sequentially and with Functory.
The step-10 suite also passed in both builds: 280 forward differential cases and
six end-to-end BRAB checks per build. `make test` passed in the main checkout,
with its per-model timeouts and an outer 180-second deadline. The main compiler
and configuration were not changed. Transaction certificates remain unverified.
