# Internal located covering

## Current implementation

Search uses `Cubetrie.Selected` for storage and `Fixpoint.FixpointTrie` for checks.
The selected module is the ordinary trie without backward transactions, and
`Cubetrie.Located` with backward transactions.
Both schedulers cover and store internal obligations, preserving location and
active bindings. Safety and approximation remain neutral-only. The obsolete
commented checker was removed rather than re-enabled.

The comprehensive acceptance suite and its assumptions are documented in
[../tx-fixpoint/README.md](../tx-fixpoint/README.md). The small suite here remains
an integration regression. The comparison below records historical results from
the earlier disabled implementation; it is not a description of current behavior.

## Historical controlled cycle experiment

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

For the current located contracts and integration checks:

```sh
make
make -f Makefile -f tests/located-covering/check.mk located-covering-check
python3 tests/located-covering/run.py
TEST_CORES=2 python3 tests/located-covering/run.py # real Functory required
```

For the historical controlled comparison, build both recorded revisions separately:

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

The unit probe checks that duplicate internal obligations are covered, while
different locations and bindings remain separate. A matching supplied invariant
must not cover them. Boundary covering remains active. The integration cases
expect SAFE for the finite internal cycle, UNSAFE for a reachable loop exit, and
a node-limit exit for the nonconvergent arithmetic loop.

Historically, the disabled-covering tests and step-8 suite passed sequentially and with Functory.
The step-10 suite also passed in both builds: 280 forward differential cases and
six end-to-end BRAB checks per build. `make test` passed in the main checkout,
with its per-model timeouts and an outer 180-second deadline. The main compiler
and configuration were not changed. Transaction certificates remain unverified.
