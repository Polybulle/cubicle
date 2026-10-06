# Located fixpoint acceptance suite

This suite exercises the implementation in
[the approved proposal](../../.hermes/plans/tx-fixpoint-implementation.md).
It assumes a population large enough for all names in each covering query.
It does not establish safety for excluded smaller populations or termination
for arbitrary transaction loops. Transaction certificates remain disabled.

## Run

From the repository root, with its opam environment active:

```sh
make
make -f Makefile -f tests/tx-fixpoint/check.mk tx-fixpoint-check
python3 tests/tx-fixpoint/run.py
make test
```

The Python runner checks verdict text and exit status. It imposes a 30-second
process-group timeout on each verifier execution and a 120-second timeout on
each direct probe. Integration models use a node limit of 40; the accounting
check uses 2 and the ordinary gapped-name regression uses 100. A limit exit is
expected only for the explicitly nonconvergent arithmetic loop, never as a SAFE
or UNSAFE substitute. Exact commands and outputs are saved in `.local/results.json`.

For real parallel execution, use a build linked to Functory. The main checkout
may use the fake library. `build-isolated.py` copies current tracked sources and
this suite into a **new** directory, configures and builds them under an existing
opam switch, and leaves the main configuration unchanged. For example:

```sh
python3 tests/tx-fixpoint/build-isolated.py "$TMPDIR/tx-fixpoint-parallel-new" --switch 4.12.0
python3 tests/tx-fixpoint/run.py \
  --binary "$TMPDIR/tx-fixpoint-parallel-new/cubicle.opt" \
  --probe "$TMPDIR/tx-fixpoint-parallel-new/tests/tx-fixpoint/.local/check.opt" \
  --cores 2 --output tests/tx-fixpoint/.local/parallel.json
```

Use `--z3` when building an isolated Z3-enabled snapshot and `--solver z3` when
running it. The build helper explicitly links Zarith, required by the installed
Z3 OCaml package but absent from the repository's default link command.

**Existing Z3 limitations are not silently counted as passes.** Its wrapper uses
untracked assertions, so SMT-derived unsat-core tags and certificate instances
are not checked on that backend. Those assertions run with Alt-Ergo. Z3's static
subtyping path can redeclare an enumeration sort; the Z3 matrix therefore uses
`-nosubtyping` and explicitly excludes the default-subtyping regression variants.
No solver-wrapper changes are included in this implementation.

## Coverage

`check.ml` exercises:

- Gapped variable normalization, fresh injective instances, initial-state checks,
  and original node/history identity.
- Different constructors, neutral-only covers, swapped argument tuples,
  control-only variables, and dummy/invalid position rejection.
- Joint coverage requiring multiple covers, extra variables, or multiple
  instances of one cover. Every full checker entry point is tested, and quick
  checks are forbidden from returning false positives.
- Control-first normalization for typed transition arguments,
  and different active values with initially identical data keys.
- Cross-support quick covering, trie compression and deletion, permutations of
  non-control variables, original deletion flags, ancestor histories, and
  cross-location cleanup.
- Ordinary certificate substitutions restored to original names, including
  fresh extra names shared consistently across the returned instances.
- Scheduler storage, duplicate covering, neutral-only safety/candidate policy,
  and internal-node accounting in both sequential and real parallel execution.
- Storage-module selection in all five transaction modes. The ordinary adapter's
  type is checked against `Node.t Cubetrie.t`. Both stores use trie compression.

The independent finite interpreter enumerates process assignments and Boolean
arrays directly, without calling production normalization or instantiation.
It checks 1,000 goal/two-cover combinations at population three. Each combination
is compared against the trie, hard SMT, naive, list, and pure-SMT entry points.
These are bounded semantic checks, not a proof for arbitrary populations.

`run.py` also tests safe/unsafe loops, supplied invariants, a nonconvergent loop,
and the ordinary gapped-name counterexample. Its matrix varies BFS/DFS,
postponement 0/1/2, and deletion enabled/disabled. The smaller pre-existing
`tests/located-covering` and `tests/neutral-candidates` suites are maintained;
their formerly disabled-covering expectations now require the finite cycle to close.

## Local correctness arguments

**Query normalization is a bijective change of bound names.** `Node.normalize`
names distinct active arguments first, in occurrence order, then the remaining
data variables in sorted order, using the canonical process prefix.
The same map acts on the atoms and the ordered argument
tuple. Original nodes, tags, mutable flags and histories are unchanged. Neutral
initial-state queries use data-only normalization; control-only variables are not
written back into cubes or carried across boundaries by this operation.

Queries use `Node.t` directly. Already normalized nodes are returned unchanged;
otherwise normalization makes a query-local copy with the same tag and history.
Support is read from the cube. Each scheduler computes one normalized node per
goal (and per replacement candidate), sharing it between safety, fixpoint, deletion,
and insertion. Parallel tasks carry that result into the worker and back into
master-side storage. Trie checker entry points require a normalized goal and do
not call normalization again. Standalone callers normalize before invoking them.
Canonical inputs take the identity fast path at the owning boundary, including
data already normalized by typing or approximation.

Located storage retains the normalized node alongside the original, reusing it
for cover instantiation. Deletion traverses normalized trie keys. Ordinary
storage remains the existing trie: its cover variables are substituted directly
into the goal's names, without first normalizing the cover. List and certificate
checks accept raw goals and normalize only those goals once per query. Their
covers retain original names. Located instantiation accounts for control-only
variables when extending the target support, without constructing renamed covers.
Prover consumes supplied nodes without further normalization. Certificates restore
only target names, using one injective inverse shared by all selected instances.
Deletion, histories, and unsafe diagnostics still use original nodes.

Data-only normalization in pre-image construction is not removed: substitution
and variable elimination occur afterwards and can invalidate its canonical
support. Concrete-state symmetry reduction is also a separate operation. Neither
is replaced by moving normalization into `Node.create`.

An isolated before/after instrumented build counted calls to `Node.normalize`
and executions of its cube-reconstruction branch (not elapsed time or total GC
allocation). With sequential Alt-Ergo, `-nosubtyping`, and a 40-node bound:

| Model | Calls before → after | Cube reconstructions before → after |
|---|---:|---:|
| `swap-safe.cub` (`-tx bwd`) | 19 → 10 | 9 → 2 |
| `loop-exit-unsafe.cub` (`-tx bwd`) | 18 → 9 | 0 → 0 |
| `nonconvergent.cub` (`-tx bwd`) | 1809 → 84 | 0 → 0 |
| `fixpoint-gapped-witness.cub` (`-tx none`) | 22 → 7 | 6 → 1 |

Verdicts and exit statuses matched. Commands and outputs are retained in
`.local/normalization-counts.json`; baseline sources, revision/diff, and the
instrumentation script are in `.local/normalization-evidence/`. Production code
contains no instrumentation. These counts establish reduced repeated work, not
a wall-clock speedup or reduced total retained memory.

**Control specialization restricts applicable cover instances.** Different
constructors cannot cover the goal. For equal constructors, corresponding
arguments fix a partial substitution from cover-bound variables to goal names.
Repeated tuple entries must agree, and distinct bound variables cannot acquire
the same image. Every injective extension over the selected finite support is
enumerated, including additional fresh names for larger covers. The data is
renamed with the same substitution. Zero-argument internal constructors also use
exhaustive enumeration; unreviewed data relevance filters are not applied internally.

**Joint SMT checking preserves the union criterion.** The goal and its full-support
distinctness are asserted once. All retained negated cover instances share that
context. No cover's alignment is imposed as a global constraint. With enough
processes, extend any goal assignment to distinct interpretations of all additional
names. Every emitted matching instance is then a valid instance of its existential
cover. A located goal state outside the union would satisfy the ground query under
that interpretation. Therefore an unsatisfiable query is a sufficient covering
check. Extra names deliberately retain the baseline SMT policy; this argument does
not establish completeness or exact behavior for small populations.

**The checkers retain their original control flow.** The list, trie, and naive
checkers each accumulate instances using their existing routines; there is no
shared replacement checking engine. The overlay supplies normalized nodes,
control-compatible substitutions, and location-filtered trie operations. The trie
retains its eager/lazy solver sequence and sorting; the list retains its early
assumptions and relevant instantiation, including in pure-SMT mode. The naive
checker uses exhaustive instantiation and one final solver check. See
[OVERLAY.md](OVERLAY.md) for the amendment's verification results.

**Transaction storage uses one compressed trie per transition name.** Typing fixes
each transition's arity and requires distinct actual arguments; call resolution
preserves distinctness. Control-first normalization therefore gives every stored
cube at that transition the same control tuple. Quick-check and deletion
permutations leave control variables fixed. Synthetic
tests use different transition names for different arities, matching typed systems.

Each trie maps normalized data to an original node and its cached normalized view;
there is no separate list of nodes or partition by support size. Data inclusion
uses the ordinary trie rule, including across different supports. Control-only
variables belong to the common control prefix. As elsewhere in this suite, unused
extra quantified variables rely on the stated sufficient-population assumption;
this is not a claim about exact small-population semantics. Hard covering traverses
the selected transition's trie directly.

Compression removes entries, not history objects, and does not mark nodes deleted.
Explicit deletion protects ancestors among the stored entries. Descendant cleanup
follows search histories across locations. Removing a compressed entry does not
restore earlier entries, just as in the ordinary trie. Approximation backtracking
starts a new search rather than restoring entries in the old store.

**Storage selection happens once at module initialization.** `Cubetrie.Selected`
selects `Ordinary` when `Options.tx_bwd` is false and `Located` otherwise, using
a first-class module with the shared storage signature. `Ordinary.t` is exactly
`Node.t Cubetrie.t`; it has no location map or retained-node list. Its node-facing
operations keep the data normalization repair. Both schedulers and both trie
checkers use the same selected abstract type. Covering remains in `Fixpoint`.

**Search integration preserves the boundary rules.** Both schedulers cover/store
internal nodes but call safety and approximation only at neutral positions.
Non-covered internal nodes still reach accounting, resource-limit checks,
predecessor generation and postponement. The parallel temporary stores use the
same located representation; each task receives the preceding snapshot, not a
snapshot already containing itself.

These are local arguments under the proposal's assumptions and the existing
Cube/SMT/pre-image contracts. They are not a machine-checked proof of Cubicle.

## Control-first trie verification

The normalized trie instantiator is separate from `instantiate_unnorm`, used via
`relevant_unnorm` by the list and certificate checkers. Internal normalized nodes
share a canonical control prefix; instantiation drops that prefix and enumerates
only the remaining variables, using the larger existing support as target. It
omits identity bindings for control variables. Ordinary/neutral behavior is unchanged.
The direct probe compares variable images and enumeration order against the raw
implementation. `make`, `make test`, and 93 bounded runs each sequentially and
with two Functory workers passed (`.local/normalized-instantiation-parallel.json`).

An allocation probe over 16 normalized pairs, repeated 1,000 times, measured
37,304,096 bytes for the raw implementation versus 19,976,096 for the normalized
one. This is an allocation comparison, not a wall-clock benchmark. Reproduce with:

```sh
make -f Makefile -f tests/tx-fixpoint/.local/normalized-instantiation.mk normalized-instantiation
tests/tx-fixpoint/.local/normalized-instantiation.opt -tx bwd -quiet -nocolor tests/tx-fixpoint/model.cub
```

Control-aware instantiation now belongs to `Instantiation.relevant` and
`Instantiation.exhaustive`, taking `~of_node` (cover) and `~to_node` (goal).
The shared implementation preserves the previous substitution order, support
extension and enumeration policy; `extend` is internal. No normalization was added.

The API move passed `make`, `make test`, and 93 bounded executions each sequentially
and with two real Functory workers (`.local/instantiation-api-parallel.json`). A
direct comparison with the frozen previous adapter checked exact substitution
lists in 200 cases per mode (`none` and `bwd`). Across 500 repetitions, allocations
were unchanged: 118,916,096 bytes in ordinary mode and 27,996,096 in backward
transaction mode. This checks allocations, not wall-clock performance.

The comparison is reproducible with `.local/instantiation-cost.mk` and
`.local/instantiation-cost.ml`; the frozen instantiation module is
`.local/old_instantiation.ml`. Commands and output are in
`.local/instantiation-cost-results.json`, with baseline sources and revision/diff
in `.local/instantiation-move-evidence/`. The first build also exposed obsolete
`mem_array` calls in the naive checker's unused helpers; these were adapted to the
node-facing storage API without removing the baseline helpers.

The normalization-boundary cleanup passed `make`, `make test`, both smaller
located-covering and neutral-candidate suites, and 93 bounded runner executions
each sequentially and with two real Functory workers. Parallel results are in
`.local/normalize-once-parallel.json`. Focused checks cover raw covers containing
control-only variables plus an extra data variable, gapped certificate source
names, original node identity, and injective restored target names.

An isolated before/after comparison under OCaml 4.12.0 counted normalization calls
and executions of its reconstruction branch. Runs used sequential Alt-Ergo,
`-nosubtyping`, and node bounds of 40 (transaction cases) or 100 (ordinary case).

| Model | Calls before → after | Reconstructions before → after |
|---|---:|---:|
| `swap-safe.cub` | 10 → 5 | 4 → 4 |
| `loop-exit-unsafe.cub` | 9 → 5 | 0 → 0 |
| `nonconvergent.cub` | 84 → 42 | 0 → 0 |
| `fixpoint-gapped-witness.cub` | 10 → 4 | 3 → 2 |

Verdicts and exit statuses matched, including the expected limit result. The
instrumentation script is `.local/normalization-boundary-measure.py`; sources,
revision/diffs, instrumented source, build logs, commands and outputs are in
`.local/normalization-boundary-evidence/`. These are call counts, not a timing
or total-allocation benchmark. Production sources contain no counters.

After removing the argument tuple and support size from the bucket key, `make`,
`make test`, and the 93-case runner passed again, sequentially and with two real
Functory workers. The new parallel record is `.local/transition-buckets-parallel.json`.
Focused assertions check cross-support lookup/compression/deletion, reject quick
matches that would move a control argument, and retain permutations of other
variables. The finite oracle now respects fixed transition arities.

The control-first normalization and list-free storage passed `make`, `make test`,
and the runner above: 93 bounded executions with sequential Alt-Ergo and another
93 with two real Functory workers in an isolated OCaml 4.12.0 build. The direct
probe includes 1,000 finite-semantics comparisons per backward-transaction mode.
Sequential output is in `.local/results.json`; parallel output is in
`.local/control-first-parallel.json`. The located-covering and neutral-candidate
suites were also rebuilt and passed. Tests were updated alongside the refactor,
not run as a separate failing-test-first cycle.

The obsolete direct gapped-witness probe has been removed: it required a plain
trie and constructor-side normalization, predating the current selected trie and
query-boundary normalization. Gapped-name contracts remain in this suite; the
gapped-witness model regressions and finite interpreter remain in their own suite.
Optional Z3 verification was not repeated for this refactor.
