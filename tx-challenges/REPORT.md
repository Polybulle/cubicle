# Transaction challenges: assessment and research direction

## Purpose and status

The aim is to develop interesting, significant models whose safety is worth
proving. A source-grounded model with an unresolved proof or encoding obstacle is
preferable to a trivialized model that quickly returns SAFE. The first drafts in
this folder failed that ambition in several ways; they are retained as controls
and as evidence of what needs rebuilding, not promoted as completed case studies.

This folder contains the existing commented models, their standalone runner and
concrete replay, the earlier research survey, and the subsequent assessments.
The ambitious replacements described below are proposals, not implemented or
verified models. No convergence speedup has been demonstrated.

Read [README.md](README.md) for model navigation and reproducible checks. The
[research survey](research-survey.md) preserves the initial literature search;
this report and the detailed assessments supersede its optimistic recommendations
for the simplified drafts. The individual `.cub` comments preserve modeling
assumptions, historical observations and source attribution.

## Findings and recommended targets

### Coherence: full-data FLASH, not artificial copying

[Detailed assessment](assessments/coherence-assessment.md)

Keep German bulk/incremental/transactional copying as a mechanism comparison.
Its incremental entry already snapshots source membership in the complement of
Done, and Flag freezes every ordinary action. Its checked permission property
cannot validate omitted snapshot work when a bad copy merely prevents completion.

The next substantial target is full-data FLASH acquisition with dirty-owner
forwarding, real invalidation and acknowledgment collection, stores, delayed
replies, request collisions, and eviction/writeback interference. Recursive
steps must process actual protocol obligations, not artificial array visits.

The existing full-data FLASH model grants exclusive permission before all
acknowledgments complete. Therefore distinguish a clean-boundary completed-flow
specification from a source-faithful early-grant model with an aggregation map.
Physical exclusive/shared exclusion is not automatically the latter's intended
all-state contract. The assessment maps the relevant source transitions and
literature to these separate targets.

HIRR remains an important hard reference. Its timeout does not disqualify it.
Missing or incomplete-looking completion branches instead require provenance
research; preserve its mechanisms rather than delete them to obtain a verdict.
Hemiola-inspired hierarchy with eviction is a longer-term structural target.

### Workflow: recover relational JobHiring

[Detailed assessment](assessments/workflow-assessment.md)

The source classifies applications by score: several winners, or no winners,
are legitimate. The draft's one-shot unique-winner election is a different
application. Identity equal to row index and publication deliberately placed
before initialization further trivialize its checks.

Rebuild around independent users, employees, categories, competence records,
multiple offers, conflict replacement, assessment, deadlines and score-derived
notification. Recursive replacement must clear precisely conflicting records
while preserving unrelated records and consistent field values. Check business
key uniqueness, competence consistency, closure and correct threshold outcomes.

The RAS and RAB variants must remain distinguished: replacement and rejecting
insertion are different relations. The RAS prose/equation conflict-key mismatch
also needs an explicit interpretation, not a silent correction. The detailed
assessment records exact source locations and language/encoding obstacles.

### Ownership transfer: repeated migration with local freshness

[Detailed assessment](assessments/ironkv-assessment.md)

Rebuild the IronKV-inspired model. Its global current-flight guards and permanent
one-delegation-per-source restriction remove the hard repeated-migration problem.
Acknowledgment cleanup does not govern reuse, and the current replay filter is
not necessary for its checked safety properties.

The substantial target retains repeated migration among the same hosts, local
sequence-number checks, old packets, meaningful acknowledgment cleanup, range
authority and client GET/SET observations. The source separates receipt/dedup/ACK
from later processing of a buffered delegation. That interval must remain
visible: a sender may clean up after an ACK while the receiver still holds an
unprocessed claim. Ownership and data must survive that schedule and later
migration, writes and stale replays.

The proposed small range domain is a modeling scope, not a theorem for arbitrary
key spaces. Fresh message identities may be needed; fresh hosts must not stand
in for repeated generations. Conservation witnesses may observe the protocol,
but must not act as an omniscient operational ownership oracle.

### Commit: competing authority and durable recovery

[Detailed assessment](assessments/commit-assessment.md)

Retain the failure-free 2PC draft as a control. Its never-reset coordinator
already supplies unique authority; splitting its local actions adds little
scientific substance.

The proposed flagship is Paxos Commit with competing leaders, intersecting
quorums, unbounded ballots, historical accepted evidence, durable/volatile
separation and real crash points. Descriptor separation is a further explicit
scope choice, not a claim that the source's one-transaction specification proves
the extension automatically.

Certificate-based finalization can be a meaningful transactional path over
multiple participant records, while distributed consensus, waiting and recovery
remain interleaved. Adding only a Down flag is not enough: recovery must preserve
chosen values and persist-before-publication obligations. Quorum/history
representation and symbolic convergence are open research problems, not reasons
to replace the protocol with a Boolean quorum oracle or unique-current-leader
assumption.

## Acceptance criteria for the next drafts

1. Preserve the defining protocol mechanisms and adversarial schedules identified
   in the assessments before attempting a proof.
2. State whether the target is a transactional committed-state specification or
   a faithful abstraction/reduction of an asynchronous source. Keep their proof
   obligations separate; multi-process transaction contracts are legitimate.
3. Make substantial safety properties depend on correct protocol work, not
   identities fixed by construction or exit guards that restate the whole goal.
4. Keep repeated operations, stale evidence and relevant interference where they
   define the problem. Declare scope bounds and justify abstractions.
5. Retain completed faulty traces and evidence that intended operations can
   complete. Finite witnesses are not unbounded correctness proofs.
6. Report unsupported syntax, missing invariants, counterexamples and timeouts
   honestly. A quick SAFE result is not the acceptance criterion.
7. Evaluate performance only after model adequacy is established; retain ordinary
   and bulk baselines where relevant and compare the intended semantics.

## Existing evidence and limitations

The pre-consolidation integrated suite matched all 17 expected native verdicts
and exit statuses. German's concrete replay checked both two-participant copy
orders, bulk final-state equality, and a faulty completed grant. These validate
the retained controls, not their scientific significance. The HIRR reference
check timed out at 100 seconds and remains inconclusive.

The parent review also executed diagnostic variants: the IronKV draft remained
SAFE without the receive-side Accepted filter; 2PC remained SAFE with annotations
ignored; moving JobHiring publication to commit changed its ignored-annotation
result from UNSAFE to SAFE. Those experiments support the critiques above, not
claims about the real systems. Original review artifacts remain in research
scratch storage and are not dependencies of this folder.

Runtime logs and caches are excluded from Git. Run the commands in README.md to
produce fresh records, including model/binary hashes, exact arguments, counters,
exit statuses and raw verifier output. No whole-project regression or proof of
source refinement is implied by this standalone model suite.
