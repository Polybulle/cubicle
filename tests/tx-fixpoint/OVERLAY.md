# Checker overlay (spec r3)

Implemented and reviewed directly by Tetra at Kes's request. This amendment
changes fixpoint.ml; it does not change storage, node normalization, certificate
back-renaming, tests, or schedulers. Earlier independent reviews are historical
evidence for their respective revisions, not reviews of this amendment.

## Structure

Removed joint_check and cover_arrays. Restored the original list and trie
check_fixpoint/check_and_add routines, hard-check exception boundaries, timing,
and diagnostics. The trie again folds directly over storage, accumulates by
prepending, sorts for eager SMT, and uses its first_action/assume/last_action
sequence. The list again uses early assumptions for close instances and relevant
instantiation even in pure-SMT mode. The naive checker again accumulates all
exhaustive instances and checks once at the end.

The overlay remains at the semantic boundaries:

- Node.covering_view normalizes data and control together without mutating nodes.
- instances supplies control-compatible substitutions; neutral queries retain
  the original relevant/exhaustive instantiators.
- assume_view asserts the normalized goal with full-support distinctness.
- Cubetrie.Selected supplies location-aware lookup/folding when needed.
- Quick list coverage requires compatible normalized positions/support. Hard
  checking handles the remaining renamings and different support sizes.

Joint coverage still uses one solver context. Internal instances are not pruned
by the data-only inconsistency heuristic. Original tags and node identities flow
into assumptions; certificate back-renaming is unchanged. The historical disabled
Internal implementation and unreachable naive quick checks were not resurrected.

## Executed checks

- make: passed, with fixpoint and callers recompiled and cubicle.opt relinked.
- make test: all 14 standard regressions passed.
- Focused matrix: 93 sequential Alt-Ergo, 93 real two-core Functory, and 92 Z3
  executions passed. The parallel and Z3 snapshots were freshly built; their
  fixpoint.ml bytes match the main source.
- Each matrix ran the direct probes in all five transaction modes, including the
  finite interpreter comparisons, union covering, invalid-position rejection,
  control-only variables, gapped names, ancestor retention, and certificates.
- Existing located-covering and neutral-candidates suites rebuilt and passed.
- git diff --check -- fixpoint.ml: passed.

The 278 matrix verdicts and exit codes match the stored selection-amendment
baseline for every command (excluding the executable path). Expected resource
limit exits remain distinct from SAFE/UNSAFE. JSON outputs are in
.local/overlay-sequential.json, .local/overlay-parallel.json and
.local/overlay-z3.json. The existing Z3 static-subtyping exclusion remains.

Tests were not weakened or edited. This is regression evidence, not a proof of
arbitrary-loop termination or exact small-population semantics. No performance
claim or new independent-review claim is made. No commits were made.
