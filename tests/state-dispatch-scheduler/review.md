# Isoqa implementation review — state-dispatch-scheduler r2

## Outcome: not approved; capability-blocked, performance decision pending

No serial correctness regression was reproduced. All executable serial frozen
checks passed. Required real parallel execution remains unavailable, including
mixed-batch handling; there is no approved exception. German's backward-enabled
runtime overhead is material (about 25–27% in these samples), not something this
review can declare small without Kes/Tetra's decision. This is not overall task
completion and not a request for an automatic production correction loop.

Kes subsequently authorized a narrowly scoped Functory installation. The preview
requires a compiler downgrade and extensive unrelated changes, so installation
was stopped before making changes. Details below. Tetra must resolve that
capability boundary before approval. Production and spec were never edited.

## Exact versions and ownership

- Specification: `.hermes/tetra-v2/tasks/state-dispatch-scheduler/spec.md`, **r2**.
- Approved baseline: `1663f808d30a39580fbab671a37da143960a779e`.
- Reviewed implementation: `fa414c97896cea8a7625986fad0070276fa7d937`.
- Original frozen tests: `7f862f754c4eed194e4902840f70926f582e6aa1`.
- Authorized cherry-pick result: `16f134d697d3b8549efff50597470be3eef43d3f`.
- Review additions/measurements: `4b02b07e8039be77390e2ae908e3bf1029f6ec94`.
- This report is committed separately; its actual commit ID is supplied in the
  handoff (a file cannot contain the hash of the commit that introduces itself).
- Branch/workspace: `state-dispatch-scheduler`,
  `/Users/hector/cubicle-state-dispatch-scheduler`.

Read local AGENTS.md, project-guide.md, r2, the role review instructions and the
implementation handoff. The frozen diff was entirely new files under
`tests/state-dispatch-scheduler/`, with no production edits or overlap; only the
explicitly authorized commit was cherry-picked. Initial outcomes in
`first-tests.md` and all frozen tests/runners remain unchanged. Review additions
are separately named `review-*`/`review.mk`. No changes to Kes's checkout or the
frozen test workspace; its baseline executable was read-only input. No merges,
pushes, history rewrites, profile changes, dependency installation, or subagents.

## Build and initial frozen outcomes

OCaml 5.4.1 in the existing switch named `5.0.0`; macOS 15.7.9 arm64, Apple M1.
Existing default configuration, Num 1.6, internal SMT, no Z3, fake Functory.
`./config.status --config` prints no extra configure arguments. Make uses
`ocamlopt.opt -dtypes -g`, num/common/smt/unix include paths and `-annot`.

Commands from this workspace:

```sh
eval "$(/opt/homebrew/bin/opam env)"
make
make -f Makefile -f tests/state-dispatch-scheduler/probe.mk isoqa-probe
python3 tests/state-dispatch-scheduler/run.py --matrix \
  --output tests/state-dispatch-scheduler/.local/review-matrix
python3 tests/state-dispatch-scheduler/run-probes.py \
  --output tests/state-dispatch-scheduler/.local/review-probes
make test
python3 /Users/hector/cubicle/.hermes/tetra-v2/shared-skills/ocaml-cubicle-development/scripts/check_regressions.py \
  --repo /Users/hector/cubicle-state-dispatch-scheduler --timeout 30
```

Build and native probe link succeeded. Frozen runner outcomes:

| Check | Actual result |
|---|---|
| Full matrix | 268 checks; 257 matched, 11 mismatched; runner exit 1 |
| Nonparallel matrix subset | 256/256 matched |
| Matrix outcomes | 147 UNSAFE, 88 SAFE, 22 LIMIT, 11 ERROR; no wall timeout |
| Structural probes | 10 passed, 3 failed; runner exit 1 |
| `make test` | exit 0; 14 OK (10 safe, 4 unsafe) |
| Independent regression status checker | 14/14 correct verdict and exit status |

Every one of the 11 matrix failures and 3 structural failures contains
`The functory library is not installed`. The remaining j2 matrix case,
`no-entry-zero-unsafe`, returns UNSAFE before invoking parallel work and is **not**
evidence of working parallelism. The frozen baseline results (154/268 matrix
matches and 0/13 probe matches) remain visible in first-tests.md; they are not
waived or relabeled as candidate results.

Serial coverage includes the safe/unsafe chains, initial-data match at internal
state, no-entry cases, zero-step detection, mixed neutral/executable siblings,
yielding alternative, identity relay with bindings, control-only variables,
empty/reset/unrelated history and kind changes, fresh neutral identity and cube
witness retention, preserved executable-step depth/history, both internal/boundary
seed orders, boundary deleted flags and visited membership, deletion on/off, all
six queues and postponement 0/1/2. Both independently forced cycle bounds and the
other bounded cycle cases return LIMIT/1, never a verdict. This does not prove
termination for internal cycles.

## Parallel inadequacy and attempted capability resolution

Minimal reproduction:

```sh
./cubicle.opt -nocolor -depth 12 -nodes 300 -tx bwd -j 2 \
  tests/state-dispatch-scheduler/models/chain-safe.cub
python3 tests/state-dispatch-scheduler/run-probes.py \
  --output tests/state-dispatch-scheduler/.local/NEW-parallel-probes
```

Expected: SAFE/0 for chain-safe, and successful structural assertions for
boundary-first, internal-first and internal-initial with real j2 workers.
Actual: ERROR/1 with missing-library message; all three j2 probes fail identically.
Raw exact commands/output/statuses: `.local/review-matrix/results.json` and
`.local/review-probes/results.json`. No fake sequential substitute was counted.

After Kes's additional authorization, ran:

```sh
/opt/homebrew/bin/opam switch show
/opt/homebrew/bin/opam list --installed
/opt/homebrew/bin/opam install --help=plain
/opt/homebrew/bin/opam install functory --show-actions
/opt/homebrew/bin/opam show functory.0.6 --field=depends
/opt/homebrew/bin/opam list --installed --short functory
/opt/homebrew/bin/opam exec -- ocamlfind query functory
```

Switch: `5.0.0`; installed compiler/base compiler: 5.4.1; ocamlfind 1.9.8.
The preview proposes **13 removals, 92 downgrades, 105 recompilations and 4
installs**, including OCaml/base-compiler **5.4.1 -> 4.14.4**, plus system packages.
Functory 0.6 requires `ocaml >= 4.03.0 & < 5.0`, ocamlfind and conf-autoconf.
This exceeds the authorized Functory/necessary-dependencies-only scope and the
explicit prohibition on compiler/removal/unrelated changes. **No install command
was executed.** The installed Functory list is still empty and ocamlfind reports
`Package 'functory' not found`. Consequently no real-Functory reconfigure/build was
possible. Tetra needs a separately authorized compatible toolchain/environment
strategy; silently relaxing the dependency bound or downgrading this switch is
not an acceptable correction. Permission to install did not itself remove the
capability blocker.

## Review additions: approximation and ordinary compatibility

```sh
make -f Makefile -f tests/state-dispatch-scheduler/review.mk isoqa-review-approx
tests/state-dispatch-scheduler/.local/review-approx.opt \
  -nocolor -quiet -tx bwd -brab 2 -depth 100 -nodes 100000 examples/german.cub
python3 tests/state-dispatch-scheduler/review-compatibility.py \
  --baseline /Users/hector/cubicle-state-dispatch-scheduler-tests/tests/state-dispatch-scheduler/.local/baseline.opt \
  --output tests/state-dispatch-scheduler/.local/review-compatibility
```

The approximation driver uses the **real** Typing, Brab, oracle, Approx and Bwd
modules, wrapping only the public CFG dispatch callback to observe nodes.
It returned exit 0:

`PASS review-approx: internal=956 boundary=43 candidate_dispatch=28 candidates=28`

All selected candidate dispatches, returned candidates and visited nodes are
neutral. German finishes SAFE; actual approximation selection was reached, not
merely the BRAB option parser. A nonquiet run is retained in
`.local/review-approx.log`; a direct native German BRAB run also succeeded in
`.local/approximation.log`. The driver does not instrument every attempted
`Approx.good` call: the skipped-internal-selection claim additionally rests on
inspection of **all three** guarded call sites in bwd.ml (82, 178, 261), with the
predicate at 46–47 and transaction.ml:115–119. Each uses lazy `if boundary then
Approx.good n else None`; an internal node cannot call Approx.good at those sites.
No selection call sites elsewhere in the scheduler were found. Parallel guards
are inspected, not dynamically verified.

The compatibility addition compares exact complete stdout/stderr and exit status
against the frozen baseline for bakery_lamport_bogus: ordinary/none/fwd/ignore,
bfs/dfs/bfsh/dfsh/bfsa/dfsa, postponement 0/1/2, deletion default/nodelete, depth
20, nodes 100, 10-second wall cap. **144/144 paired configurations identical**:
32 UNSAFE, 112 LIMIT, no errors/timeouts. Limits establish bounded compatibility,
not successful verification. Raw paired records are in
`.local/review-compatibility/results.json`.

## Source and caller audit

The implementation diff is restricted to transaction.ml, pre.ml and bwd.ml;
public interfaces are unchanged. Read transaction/pre/bwd interfaces and relevant
implementations plus node.ml, typing.ml, approx.ml, brab.ml, safety.ml,
fixpoint.ml, cubetrie.ml, stats.ml and options.ml.

- **Dispatch:** transaction.ml:107–113 obtains Before event, formal predecessors,
  substitution and available variables from state plus cube witnesses. No `from`
  or `kind` dependence remains. CFG construction preserves predecessor order
  (60–75); filling calls retains distinct wildcard selection and noncontiguous
  fresh-variable scope (78–105). The dispatch probe tests reversed control-only
  argument permutation and unrelated diagnostic history.
- **Neutral crossing:** pre.ml:343–363 handles each call in the same fold. Neutral
  uses Node.create with the same cube/kind, explicitly copies full from/depth,
  clears position bindings, bypasses transition lookup, and conses only into
  immediate work; executable siblings still go through normal pre/cube logic.
  Node.create (100–112) gives a fresh tag/object and nondeleted flag. Pre's final
  reversal preserves each output-list order. No fictitious history step appears.
- **Executable positions:** pre.ml:238–288 substitutes actual arguments, constructs
  Before transition with those arguments and Node.create's executable history.
  Its postponement rules are unchanged. Neutral transfer has no executable
  update/additional witnesses, so goes directly to immediate work; executable
  post lists still use the existing reverse-append/drain protocol.
- **Sequential boundaries:** bwd.ml:69–113 guards safety, all fixpoints,
  approximation selection, deletion and visited insertion. Limits/new-node
  accounting run on internal expansion. Safety still precedes covering at neutral,
  preserving zero-step unsafe. Supplied candidates go through the same queue.
- **Parallel path:** the selected implementation (305–309) is gentasks_hard ->
  worker_fix -> master_fetch, not the older full worker helper. Batch preparation
  performs safety/easy covering only for boundary nodes and never inserts internal
  obligations into transient visited tries. worker_fix bypasses hard covering for
  internal nodes; WR_NoFixpoint checks resource limits and guards approximation;
  populate_pre guards deletion/insertion. The alternate gentasks/full-worker
  helpers have matching boundary guards too. `do_sync_barrier=true` remains
  unchanged. This control-flow audit is not a substitute for real worker runs.
- **Global invariants:** typing.ml:589–603 constructs supplied invariants at
  Node.dummy_pos and unsafe seeds at neutral. bwd.ml:66–67 and 296–297 insert
  supplied invariants unchanged as the explicit r2 exception; they are not queued
  for CFG dispatch. Typing's init instances retain invariant constraints. Internal
  nodes bypass covering even against invariants; boundary nodes retain existing
  invariant covering/deletion behavior. No new global-invariant semantics were
  introduced or asserted by the probes.
- **Deletion:** Cubetrie.delete_subsumed (305–321) mutates deleted flags and filters
  descendants; FixpointTrie.easy_fixpoint (368–371) consults them. The new guards
  prevent any internal obligation initiating that operation or becoming a cover.
  This is boundary-only covering, not general state-aware subsumption. Boundary
  deletion/ancestor behavior remains existing policy, including for supplied
  invariants; initial invariant insertion is not prohibited internal insertion.
- **Approximation:** approx.ml:279–284 preserves position when making a candidate;
  thus a selected neutral approximation remains neutral. Brab initializes the
  real oracle and handles restarts/candidate origins; neutral crossing preserves
  history for Node.origin. Candidate identity/general located covering remain
  deferred. The unused full-worker candidate/preimage asymmetry at bwd.ml:185 is
  pre-existing and is not claimed fixed by this change.
- **Ordinary path:** `not tx_bwd || ...` short-circuits without CFG predicate calls.
  The guarded statements execute in the old order; Pre selects unchanged normal
  preimage logic at initialization. Forward code is untouched. The exact paired
  output comparisons below and the queue compatibility run support this audit.

These are bounded tests and code arguments, not a soundness/completeness proof.
Skipping internal covering preserves obligations but may worsen termination and
resources on cycles. No general approximation identity, state-aware covering,
forward neutral traversal, or parallel BRAB backtracking correctness is claimed.

## Controlled matched serial benchmark (before attempted installation)

Completed before the install preview, with both binaries using identical OCaml
5.4.1/default configure/internal SMT/fake-Functory settings. All builds had stopped;
no concurrent build/benchmark was seen in the process snapshot. The machine was
on battery, 68% at benchmark start (64% at later post-check), with no thermal or
performance warning recorded by `pmset -g therm`. It was not an idle dedicated
host: load averages were 2.12/2.32/2.26 and Emacs/WindowServer/Hermes/background
services were present. No power settings were changed. Outliers are retained.

Verified baseline SHA-256 against first-tests.md:
`696377ac02ce00b2694351006baef9bb4658c3a4081e50cffac20f38aa911777`.
Candidate executable SHA-256:
`5208baabf2a5546836183cf4cc511f5f1a0c4831d6f744f759e697817552872d`.
Production matches the submitted source commit exactly; test additions do not
change the benchmarked executable.

```sh
python3 tests/state-dispatch-scheduler/benchmark.py \
  --baseline /Users/hector/cubicle-state-dispatch-scheduler-tests/tests/state-dispatch-scheduler/.local/baseline.opt \
  --candidate /Users/hector/cubicle-state-dispatch-scheduler/cubicle.opt \
  --baseline-revision 1663f808d30a39580fbab671a37da143960a779e \
  --candidate-revision fa414c97896cea8a7625986fad0070276fa7d937 \
  --repetitions 7 --timeout 120 \
  --output tests/state-dispatch-scheduler/.local/controlled-preinstall
python3 tests/state-dispatch-scheduler/review-analysis.py
```

Frozen four ordinary models x six modes; one warmup per binary/group excluded,
seven paired repetitions, randomized binary order (seed 1663): **384 executions,
24 groups**, including 336 measured runs. Every invocation is `/usr/bin/time -l
BINARY -nocolor -depth 100 -nodes 100000 MODE ABSOLUTE_MODEL`; 120-second wall cap,
no RSS cap. Model hashes and exact commands in metadata/raw JSONL. No BRAB or
forward oracle is requested, so bare tx does not add forward exploration costs.

Raw logs: `.local/controlled-preinstall/{metadata.json,runs.jsonl,summary.json}`.
Committed `review-measurements.json` retains group medians/ranges, per-process
real/user/sys samples, RSS, counts, outcomes, complete error traces, paired wall
deltas and full-verifier-output equality. The frozen runner missed time(1)'s
locale decimal commas and only extracted first lines of wrapped traces. The
**review addition** reparses preserved raw outputs without rerunning or modifying
initial results, handles commas, and compares every verifier line after removing
only time(1)'s trailer. No silent test weakening.

### No-tx compatibility

| Model | Baseline median ms (range) | Candidate median ms (range) | Delta | Baseline/candidate median RSS bytes |
|---|---:|---:|---:|---:|
| bakery | 12.993 (12.621–13.173) | 12.955 (12.889–13.136) | -0.29% | 13320192 / 13336576 |
| german | 3244.573 (3222.251–3427.930) | 3308.660 (3244.982–3720.687) | +1.98% | 123060224 / 125386752 |
| bakery_lamport_bogus | 46.993 (46.707–48.544) | 46.761 (46.510–47.855) | -0.49% | 19562496 / 19529728 |
| swimming_pool | 90.869 (90.494–92.347) | 90.851 (89.550–92.776) | -0.02% | 25821184 / 25968640 |

All **16 backward-disabled groups** (ordinary/none/fwd/ignore) have identical
complete verifier outputs and statuses in all seven pairs, including wrapped
traces, ordering, counts and diagnostics. Their same-mode median deltas range
from -3.10% to +2.47%, with overlapping noise and isolated outliers; no convincing
ordinary-path regression is demonstrated by these measurements. All runs reached
SAFE/0 or UNSAFE/1 in those groups.

### Candidate backward-enabled overhead versus candidate ordinary

| Model | bwd median ms / overhead | bare tx median ms / overhead | bwd / bare median RSS bytes |
|---|---:|---:|---:|
| bakery | 12.983 / +0.22% | 13.463 / +3.93% | 13516800 / 13516800 |
| german | 4118.822 / +24.49% | 4197.428 / +26.86% | 121618432 / 123633664 |
| bakery_lamport_bogus | 51.159 / +9.40% | 50.381 / +7.74% | 19857408 / 19841024 |
| swimming_pool | 91.134 / +0.31% | 90.866 / +0.02% | 26296320 / 25837568 |

German adds approximately 0.810/0.889 seconds; its bwd range is 4.107–4.289 s
and bare range 4.150–4.240 s, versus ordinary 3.245–3.721 s. That is material beyond
observed ordinary timing noise. Bogus bakery adds about 4.398/3.620 ms. The tiny
bakery and swimming-pool changes are near process-start/timing noise. No agreed
numeric acceptance threshold exists; these are measurements and a concern for
Tetra/Kes, not a fabricated threshold or automatic failure label.

All **eight baseline bwd/bare groups fail with `Fatal error: Not_found`, exit 1**.
All candidate bwd/bare runs give the expected safe/unsafe verdict. Same-mode
numbers are retained but flagged noncomparable: repairing a crashing baseline is
not a speedup, nor a meaningful same-mode performance regression ratio.

Candidate ordinary -> bwd counts (visited expansions / fixpoints / solver calls /
max processes / deleted / restarts):

| Model | Ordinary | bwd (bare same counts) |
|---|---|---|
| bakery | 2 / 7 / 2 / 2 / 0 / 0 | 10 / 7 / 2 / 3 / 0 / 0 |
| german | 2384 / 32068 / 50631 / 3 / 278 / 0 | 36588 / 31869 / 48381 / 4 / 268 / 0 |
| bakery_lamport_bogus | 74 / 87 / 819 / 2 / 8 / 0 | 242 / 87 / 824 / 2 / 8 / 0 |
| swimming_pool | 79 / 74 / 3155 / 1 / 1 / 0 | 319 / 74 / 3155 / 1 / 1 / 0 |

The visited statistic counts every expansion (Stats.new_node), not covering-trie
membership; internal expansions explain much of its increase. Explicit control
bindings/call scope and new neutral boundary scheduling also change witnesses,
order and pruning counts; equality with ordinary is not required for tx-enabled
counts. German's differences are recorded rather than attributed entirely to one
cause. Unsafe swimming_pool retains `Init -> t8() -> t1() -> unsafe[2]`. Bogus
bakery's six executable steps retain their form with process identities #1/#2
swapped between ordinary and bwd/bare; there is no fictitious neutral step. Exact
backward-disabled traces are unchanged.

## Remaining acceptance obligations / handoff

1. **Capability:** provide an authorized Functory-compatible environment without
   the unapproved switch changes above; reconfigure/rebuild there and repeat real
   parallel feature, both-order mixed batch, initial-intersection and approximation
   checks. No installation success or parallel approval is claimed.
2. **Performance:** Tetra/Kes decide whether measured ordinary-model tx overhead,
   especially German, meets r2's qualitative requirement or requires scoped work.
   If optimization is requested, preserve the boundary-only semantics and repeat
   affected measurements. There is no demonstrated serial functional defect for
   Octa to repair in this review.
3. **Limitations:** supplied invariant interactions were source-audited, not an
   exhaustive proof; deferred TODOs and general cycle termination remain outside
   this task. The inactive parallel helper and BRAB caveats are not certified.

Review additions and this report are the only new writes intended for commit.
Final checks include `git diff --check`, staged diff checks, clean production diff
against fa414c9, and no continuing builds/writers. All activity stops at handoff.
Tetra alone decides final acceptance; this report does not mark the task complete.
