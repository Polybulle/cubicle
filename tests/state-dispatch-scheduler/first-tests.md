# Frozen independent tests: state-dispatch-scheduler

## Authority and provenance

- Isoqa test-design phase only, specification **r2**, read from
  `.hermes/tetra-v2/tasks/state-dispatch-scheduler/spec.md`.
- Baseline: `1663f808d30a39580fbab671a37da143960a779e`.
- Workspace: `/Users/hector/cubicle-state-dispatch-scheduler-tests`;
  branch: `state-dispatch-scheduler-tests`.
- Expected results derive from r2, not implementation observations. Only this
  workspace's baseline source, public interfaces, examples, specification, and
  role instructions were inspected. No implementation branch, report, diff, or
  Kes untracked tests were inspected. The frozen commit is identified in the
  handoff; this report cannot contain its own commit hash.
- Production/specification files unchanged. Local binaries, build output, raw
  run logs and JSON are excluded from the test commit by `.gitignore`.

## Reproduction

Run from the test workspace (or an explicitly authorized integration workspace):

```sh
eval "$(/opt/homebrew/bin/opam env)"
# Only when configuration is absent:
autoconf && ./configure
make
make -f Makefile -f tests/state-dispatch-scheduler/probe.mk isoqa-probe
python3 tests/state-dispatch-scheduler/run.py --matrix \
  --output tests/state-dispatch-scheduler/.local/review-matrix
python3 tests/state-dispatch-scheduler/run-probes.py \
  --output tests/state-dispatch-scheduler/.local/review-probes
make test
```

Use a new output directory for each invocation. Runners refuse to overwrite one.
The feature runner and probe runner return nonzero for mismatches; baseline
failures are **not** converted into passes. They require verdict text and the
associated exit status. A limit is `Reached Limit` with status 1, not UNSAFE.
Errors, wall timeouts, and resource limits remain separate categories in JSON.
The black-box matrix normally uses depth 12, node limit 300, and a 20-second
external wall timeout; the whole process group is killed on timeout. Two cycle
runs isolate depth 3/node 300 and depth 100000/node 6. No memory cap is imposed.

`probe.mk` links the real production modules, excluding `main`, with a test-only
OCaml driver under `.local/`. No production source is patched or mocked. For
selected tests only, a `cfg` record is supplied through the public `t_system`
interface to isolate predecessor handling or scheduling. Each probe uses a fresh
process because Options and other production modules initialize global state.
Scheduler dispatch observations use a per-invocation append-only file, not a
process-local reference that would lose events from parallel workers.

## Requirement-to-test mapping and expected outcomes

| r2 contract | Test | Expected outcome / discrimination |
|---|---|---|
| Minimal executable chain and entrypoint | `chain-unsafe.cub`, `chain-safe.cub` | UNSAFE/1 and SAFE/0 respectively; enter then finish is the only transaction chain. The safe entry requires Bad, unreachable from Idle. |
| Internal safety skipped | `internal-init-safe.cub` | SAFE/0: finish's predecessor has initial data Idle, but its only entry requires Bad. Checking that internal cube produces a false alarm. |
| No entries, zero-step unsafe | `no-entry-zero-unsafe.cub` | UNSAFE/1 before any predecessor is required. The only transition is triggered. |
| No entries, initially safe | `no-entry-safe.cub` | SAFE/0 despite an internal predecessor matching initial data; no entry reaches it. |
| Neutral call independent of executable siblings | `mixed-unsafe.cub`; `mixed` probe | UNSAFE/1 requires following enter as well as the neutral predecessor of finish. Probe explicitly supplies both call orders, forbids neutral lookup and requires both outputs with depths 0/1. |
| Yielding branch | `yield-unsafe.cub` | UNSAFE/1 at Mid through the yield alternative, without executing finish. |
| Bindings survive executable steps | `bindings-unsafe.cub` | UNSAFE/1 after enter(p), relay(p), finish(p). Relay is an identity step, so cube-only covering incorrectly drops needed work. |
| Control-only variables and position dispatch | `control-only.cub`; `cfg-state` probe | UNSAFE/1; probe uses a variable-free cube and noncontiguous process identities, expects the exact reversed permutation enter(q,p) from Before relay(p,q). |
| History/kind independence | `cfg-state` probe | Same parent calls for Orig/Node/Approx with empty history or deliberately unrelated history. Neutral Node with reset history must enumerate yielding finish, not fail or enumerate entry. |
| Neutral short-circuit, fresh identity, witnesses, bindings | `neutral-transfer` probe on array model | Neutral transition lookup is a failing sentinel. Result must have fresh tag/object, equal cube including witnesses, identical history/depth, and `Node.neutral_pos` with empty active bindings. |
| Executable trace depth | `trace` probe on chain | finish gives depth 1; enter gives depth 2; neutral crossing retains exactly two trace steps and identical history. |
| Internal fixpoint skipped; boundary-only visited/deletion | `boundary-first`, `internal-first`, `internal-initial` probes | Both equal-cube internal/boundary obligations must reach dispatch; the internal one is absent from returned visited; boundary remains present and not deleted. An internal general cube matching init must not raise Unsafe or delete boundary. |
| Parallel batch boundary handling | Same scheduler probes with `-j 2`; all models with `-j 2` | Same semantic contracts. Both input orders target batch preparation, not only worker handling. Real execution currently blocked by missing Functory. |
| Deletion compatibility | All models and scheduler probes with `-nodelete`, plus default deletion | Same verdicts/contracts with and without deletion. Structural probes check the boundary deleted flag and returned visited list. |
| Queues/postponement | Every model under bfs/bfsh/bfsa/dfs/dfsh/dfsa and postpone 0/1/2 | Same verdict; array model supplies additional-process preimages. These are option/semantic checks, not assertions of a particular internal queue operation count. |
| Approximation | Four models with `-tx bwd -brab 2` | Same expected verdicts for chain-safe, chain-unsafe, internal-init-safe, ordinary-safe. Does not prove an approximation is selected or that internal selection is never attempted: review gap below. |
| Internal cycles and resource checks | `internal-cycle.cub`, ordinary and separately forced bounds | LIMIT/1, never SAFE/UNSAFE. This specific self-loop has no entry or terminating backward branch; its repeated Bad internal obligations must not be closed by an internal fixpoint. This is not a general termination claim. |
| Backward-disabled compatibility | Ordinary safe/unsafe with no tx, none, fwd, ignore; existing `make test` | SAFE/0 and UNSAFE/1 respectively; compare production runs before/after, including counts and trace order. |
| Ordinary inputs with backward enabled | Ordinary safe/unsafe with bwd and bare tx; benchmark procedure | Same verification verdicts. Count differences introduced by boundary steps are recorded, not automatically treated as wrong for backward-enabled modes. |

The scheduler probes supply a terminating graph with no predecessor calls, and
seed both a boundary and an internal obligation. They test the scheduler's
contract independently of graph reachability; they are not claimed to be parsed
whole-model executions. For the initial-intersection variant, the internal cube
is true, which also exercises the potential for internal subsumption/deletion.
Supplied global invariants are intentionally not ruled out by the visited check;
these particular fixtures supply none.

## Actual baseline results

Build: `autoconf && ./configure && make` succeeded. Compiler: OCaml 5.4.1 via
`/opt/homebrew/bin/opam env`. Configure found num, did not find Functory, and
compiled without Z3. Native host: macOS 15.7.9 arm64, Apple M1. This is a baseline
build, not an implementation build.

Retained local baseline executable: `.local/baseline.opt`, copied from this
workspace's successful `cubicle.opt` build before the test commit. SHA-256:
`696377ac02ce00b2694351006baef9bb4658c3a4081e50cffac20f38aa911777`.
Rebuilds need not reproduce the byte hash (version/build metadata); source and
configuration provenance remain mandatory. Probe build also succeeded.

Base feature cases, each `-tx bwd -depth 12 -nodes 300 -nocolor`:

| Model | Expected | Observed baseline |
|---|---|---|
| chain-safe | SAFE/0 | SAFE/0 |
| chain-unsafe | UNSAFE/1 | UNSAFE/1 |
| internal-init-safe | SAFE/0 | UNSAFE/1: expected baseline failure |
| no-entry-safe | SAFE/0 | UNSAFE/1: expected baseline failure |
| no-entry-zero-unsafe | UNSAFE/1 | UNSAFE/1 |
| mixed-unsafe | UNSAFE/1 | ERROR/1, `Fatal error: Not_found` |
| yield-unsafe | UNSAFE/1 | UNSAFE/1 |
| bindings-unsafe | UNSAFE/1 | SAFE/0: internal identity-step covering loses the path |
| control-only | UNSAFE/1 | UNSAFE/1; structural dispatch probe still fails |
| internal-cycle | LIMIT/1 | SAFE/0: internal fixpoint, not evidence of termination under r2 |
| ordinary-safe | SAFE/0 | SAFE/0 |
| ordinary-unsafe | UNSAFE/1 | UNSAFE/1 |

Final black-box matrix: **268 checks, 154 matched, 114 mismatched**. Outcomes:
148 UNSAFE, 89 SAFE, 31 ERROR; no actual LIMIT or external TIMEOUT. The final
logs/commands/statuses are in `.local/final-matrix/results.json`. No result is
an implementation result. Expected baseline failures reflect missing r2 behavior;
missing-library failures are capability gaps, not feature failures. The `-j 2`
zero-step case can finish before calling Functory; this does not validate parallel
execution. Some other parallel cases fail before the library call as well.

Structural suite: **13 probe invocations, all failed on baseline**:

- cfg-state: wrong calls from state with empty history.
- neutral-transfer and mixed: sentinel reports forbidden neutral lookup.
- trace: `Not_found` at the neutral crossing.
- boundary-first (default and nodelete): internal obligation covered.
- internal-first (default and nodelete): internal obligation covered boundary.
- internal-initial (default, nodelete, j2): internal initial-data match ran safety.
- boundary-first/internal-first j2: `The functory library is not installed`.

Detailed probe evidence: `.local/frozen-probes/results.json` (probe build returned
0; probe runner returned 1, as expected for this baseline).
`make test` returned 0 and reported **14 OK**, including safe and unsafe examples;
raw output is `.local/make-test.log`. Passing existing regressions does not
establish transaction correctness. Initial model-authoring smoke exposed a syntax
error in the relay guard; the corrected frozen fixture uses `X[q] = Mid`, parses,
and exhibits the baseline false-SAFE result above.

## Performance comparison procedure (frozen, not an acceptance threshold)

Build baseline and the submitted candidate with identical OCaml, libraries,
configure flags, optimization, and environment. Finish all builds first. Retain
both executables and record source commits, clean production status, and hashes.
Do not run a benchmark while either worker builds or while other heavy jobs run.
Use one host, fixed power mode, no other benchmark, and record load/thermal or
background activity. Re-run a session if external load dominates; retain and
explain the first session rather than silently discarding inconvenient runs.

```sh
python3 tests/state-dispatch-scheduler/benchmark.py \
  --baseline tests/state-dispatch-scheduler/.local/baseline.opt \
  --candidate /absolute/path/to/reviewed/cubicle.opt \
  --baseline-revision 1663f808d30a39580fbab671a37da143960a779e \
  --candidate-revision EXACT_SUBMITTED_COMMIT \
  --repetitions 7 --timeout 120 \
  --output tests/state-dispatch-scheduler/.local/controlled-comparison
```

Inputs: bakery, german, bakery_lamport_bogus, swimming_pool (tracked ordinary
models). Modes: no tx, none, fwd, ignore, bwd, and bare tx. One warmup per binary
and input/mode is excluded; seven measured repetitions follow, pairing the two
binaries in a deterministic randomized order (seed 1663). Limits: depth 100,
100000 nodes, 120-second wall timeout per process; no RSS cap. This script is
macOS-specific because it uses `/usr/bin/time -l`; a Linux port needs explicit
unit/format handling, not silent reuse.

Records contain commands, verdict plus exit status, complete search/trace output,
visited/fixpoint/deletion/solver/process/restart counts, wall time, time(1) real/
user/system time, maximum RSS in bytes, hashes, source revisions, host and load.
Summary reports medians, min/max noise range, runtime percentage/absolute deltas,
RSS deltas, and exact behavior comparison. Retain raw samples; investigate search
count or trace differences, not just aggregate timing. The runner does not decide
performance acceptance. ERROR/TIMEOUT/LIMIT samples are not performance evidence
for equivalent verification, even if their elapsed times or outcomes happen to
match; `verdict_samples_only` marks this distinction.

Compare baseline vs candidate for no-tx and all backward-disabled modes first:
ordering, traces, counts and verdict/status should be preserved. For bwd/bare,
check verdict/status and executable traces, explain any count differences, and
report both the same-mode baseline delta and candidate bwd/bare vs candidate
no-tx overhead on the identical model. A broken baseline tx run is not an
optimization target and cannot justify a speedup claim. No numeric tolerance has
been agreed; give Tetra measurements and noise for Kes's decision.

These benchmark commands do not enable BRAB/forward exploration. Bare tx enables
both switches, but this configuration isolates backward traversal costs rather
than changing the forward oracle. The functional BRAB smoke is separate. A later
BRAB performance experiment must be reported separately, with the same explicit
forward parameters and separate forward/backward interpretation.

Actual runner exercise: both labels pointed to the **same retained baseline**,
`--runner-smoke --repetitions 1`, using ordinary-unsafe. It completed 24 process
runs over six modes, all six behavior comparisons equal; RSS was captured for
both labels in every group. Evidence: `.local/final-benchmark-smoke/`. This tests
capture mechanics only, **not overhead or candidate performance**. Machine load
was nontrivial and another task may have been building; no controlled performance
claim is made. Full repeated controlled measurements remain review work.

## Gaps and requests to Tetra

1. **Parallel capability blocker:** Functory is absent. j2 runs were attempted,
   not passed. Supply an authorized Functory-capable environment before accepting
   real parallel batch/worker behavior. No dependency installation or fake
   concurrency substitute was performed.
2. **Approximation timing:** BRAB option/verdict smoke is present, but no fixture
   demonstrates successful approximation selection at a neutral boundary and
   prohibited selection at an internal node. Review must inspect/instrument the
   actual selection sites after the frozen handoff. This is not covered by the
   four passing-or-failing verdict smoke runs alone.
3. **Postponement and deletion details:** options, identity-step counterexamples,
   both scheduler seed orders, visited results and deleted flags are checked.
   Exhaustive operation timing and supplied-global-invariant interaction still
   require source review; no claim of general state-aware covering or candidate
   identity (deferred TODO points 7/8) is made.
4. **Performance acceptance decision:** r2 explicitly provides no numeric
   threshold. Tetra/Kes must decide what measured ordinary-model overhead is
   small after controlled candidate measurements; no tolerance was invented.
5. Interface-based probes freeze the baseline public contract. If a permitted
   interface adjustment prevents linking, report it to Tetra rather than silently
   altering frozen expectations. Later additions must be labeled review additions.

No implementation approval or overall-task acceptance is given. The suite is
frozen for transfer only after Tetra authorizes it and both writers stop.
