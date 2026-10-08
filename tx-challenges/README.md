# Commented transaction models

These are the original, deliberately retained drafts and controls, not completed
ambitious case studies. Start with [the assessment report](REPORT.md) and its
linked detailed assessments before using any model in a scientific presentation.
The [research survey](research-survey.md) records the earlier candidate search;
the later assessment supersedes its optimistic recommendations for these drafts.

The presentations are inside the `.cub` files: introduction, abstraction and
assumptions, safety predicates, transaction boundaries, phase explanations,
observed development outcomes, and limitations.

Read these first:

- `ironkv/ironkv-safe.cub`: single-key ownership/data transfer with distinct local
  send and receive transactions. One delegation identity per source host;
  not unrestricted repeated migration or a verified IronKV implementation.
- `german/german-incremental-tx.cub`: actual incremental directory copying with
  a changing-participant loop. Compare `german-incremental.cub` and
  `german-bulk.cub`; network invalidations/acknowledgments remain asynchronous.
- `jobhiring/jobhiring-safe.cub`: committed insertion consistency and recursive
  outcome classification under an explicit serialized workflow contract.
- `2pc/two-phase-commit.cub`: preparation, abort races and decision propagation,
  with local handlers rather than an atomic distributed commit.
- `hirr/hirr-pvcoherence-reference.cub`: commentary-only reference; executable
  contents match the existing HIRR model. No new annotation was justified.

Every new family has an unsafe mutant. Completion-query files deliberately
replace safety goals with reachable completed states: their expected UNSAFE
verdict is a positive reachability result, not a protocol defect.

## Reproduce

From the repository root, using the configured current checker:

    python3 tx-challenges/run.py
    python3 tx-challenges/german/replay.py

The runner executes 17 functional checks sequentially with explicit transaction
modes, node limits and a 30-second external process-group timeout per attempt.
It writes full logs, JSONL results with counters/hashes/argv/exit status, and a
summary into a fresh `.local/run-*` directory. `--binary`, `--output`, and
`--timeout` can override defaults. It does not build, retry, warm up, change the
benchmark manifest, or rerun the timed-out HIRR reference. German uses BRAB2
for candidate rejection; this is not a two-process safety theorem.

The narrow German concrete replay evaluates these known model files and asserts
actual guards, simultaneous updates and call successors. It exercises both
orders of two copy participants, completed invalidation/acknowledgment/grant,
bulk final-state equality, and a concrete faulty committed grant. It is neither
a general Cubicle parser nor an unbounded equivalence proof. Output is in
`german/.local/concrete-traces.json`.

Parent acceptance: all 17 integrated-model checks matched native verdict and
exit status in `.local/run-20261008-121511/`; concrete replay passed. HIRR's
builder check timed out at 100 seconds and remains inconclusive. No timing
speedup or necessity of internal covering is claimed. Existing production code,
benchmark recipes, and other models were not edited for this suite. This is a
standalone model suite, not yet wired into `make test`.

Original builder exploration, including failed attempts, remains under
`/Users/kes/.hermes/profiles/tetra/cache/scratch/txmodels-*/`; the model comments
label those commands/results historical. Those scratch files are not required
by the integrated runner or concrete replay.
