# Commit-model assessment: replace the flagship, retain the 2PC control

> Relocation note: this assessment was written before consolidation.
> `tests/transaction-models/` is now `tx-challenges/`;
> `experiments/transaction-benchmark-ideas.md` is now
> `tx-challenges/research-survey.md`. Historical model line references
> retain their numbering. Source snapshots mentioned below were research
> scratch material, not runtime dependencies; primary-source URLs remain
> in each report. No proposed replacement has been implemented here.

## Verdict

**Replace the current 2PC draft as the ambitious research target with crash-recovering, competing-leader Paxos Commit, including two distinct transaction descriptors and an unbounded ballot domain.** Retain the existing four files as a documented one-shot safety/control suite. Do not call merely splitting or regrouping their actions a recovered scientific contribution.

This recommendation is a specification proposal, not an implementation, verified theorem, or performance result. It deliberately accepts a difficult first draft whose safety proof may initially be incomplete. The scientific substance is preservation of chosen participant decisions across intersecting quorums, durable recovery, concurrent ballots and delayed messages, composed into atomic-commit validity and agreement. A transaction feature demonstration is secondary; the protocol must remain interesting even with transaction annotations ignored.

Primary anchor: Gray–Lamport §§2–4, §6.1, §6.4 and Appendix A.3, whose complete `PaxosCommit` TLA+ safety specification is printed in the paper.[1] Two distinct descriptors extend its single-transaction appendix using the paper's explicit descriptor requirement; they are not claimed to be covered by its printed theorem without a composition argument.[1]

## What the present artifacts actually preserve and remove

Read all four `tests/transaction-models/2pc/*.cub` files, the earlier research notes, `transaction.ml`, and the primary paper. Repository starting revision: `3661c27faed192c0a639d395fe10f6ac35650516`; worktree already dirty. No model/checker runs, builds, installs, source edits or commits were performed. The parent reports SAFE with `-tx ignore`; that observation was not independently rerun here.

Artifact references below are to `tests/transaction-models/2pc/two-phase-commit.cub` unless stated otherwise.

- Lines 20–34 explicitly restrict the system to one transaction, a nonfaulty scalar coordinator, fixed participation, no crashes/recovery/IDs/resources, and immutable logical message slots. These restrictions are honestly documented, not concealed assumptions.
- Lines 121–153 retain RM prepare/reject versus coordinator receipt; lines 160–192 retain abort versus vote collection, and partial dissemination; lines 198–223 retain separate participant outcome reception. This is not an atomicized entire network execution.
- Lines 107–116 check RM agreement, preparation history and terminal stability. `EverPrepared` records an actual modeled event, but is not a durable prepare log. `FirstDecision` and `FirstRM` record first terminal events, but no reset or recovery action threatens them.
- `choose_commit` has an all-RM receipt guard, whereas `choose_abort` is enabled only while undecided. Consequently outcome uniqueness is directly enforced by a single never-reset scalar. Participant terminal guards then protect first-outcome records. The remaining validity reasoning is chiefly the chain `SeenYes -> VoteYes -> EverPrepared`.
- The missing-vote mutant removes only the all-other-RM guard. Its comments retain a completed conflict trace; the two probes preserve the protocol while replacing only the query with two-RM commit or prepared-then-abort reachability. These are useful controls, not substantial failure/recovery evidence.
- Source Appendix A.2 itself deliberately abstracts message loss as nondelivery and duplicates as repeated access to an ever-sent message set, and omits TM Prepare messages and RM Abort messages.[1] Thus the current monotone network representation is not intrinsically an illegitimate simplification. It becomes inadequate when one adds identities/reclamation or non-idempotent effects without revisiting its adequacy.
- A.2's participant commit-receive action is enabled by a Commit message, without the draft's extra prepared/first-terminal checks.[1] In correct executions these checks should be derivable. In mutants they can mask bad coordinator behavior; source-faithful and defensive-handler variants should not be conflated.

**Lost substance:** actual durability ordering and state loss, uncertain recovery, multiple processes believing themselves coordinator/leader, accepted-history preservation across ballots, and cross-descriptor replay. **Not lost from this paper's core safety scope:** SQL isolation, resource values, arbitrary inter-transaction conflict resolution, dynamic membership, and infinite transaction-ID recycling. Those require separate specifications.

The earlier survey's advice to start with compact local 2PC handlers prioritized implementation feasibility. It should not determine this ambition-first choice. The current model is a credible compact encoding of the simplified 2PC safety skeleton, but it is not a flagship proving that transactions address a significant protocol problem.

## Which proposed additions restore central difficulty?

| Addition | Assessment and consequence |
|---|---|
| Crashes that only disable execution while all protocol state remains intact | Faithful to the paper's abstract failure-as-pause model, but add little safety difficulty; primarily expose blocking/progress scenarios. A `Down` flag alone is not recovery science. |
| Stable/volatile separation, crashes losing volatile state, restore from logs | Central when the model tests persist-before-send. Gray–Lamport §3.1 requires recording state before sending messages from it; safe recovery then implements a pause.[1] Show that correspondence instead of assuming it. |
| Durable prepare before Yes/Prepared publication | Central: a forgotten Yes allows a recovered RM to reject after others commit. `EverPrepared=True` alone cannot rule that out. |
| Durable 2PC coordinator outcome before publication | Central for recovered 2PC: a forgotten published Commit lets its restarted TM choose Abort. In Paxos Commit, a leader's outcome cache is not the ultimate durable authority: acceptor records and fresh quorum recovery are. Do not impose redundant leader fsync merely to make the example bigger. |
| In-doubt participants and outcome queries | Central failure scenario; safety requires no unilateral abort after durable prepare. Permanent uncertainty is allowed by safety, not an unsafe state. Queries are meaningful only if responses derive from durable/certified decisions. |
| Same 2PC coordinator restarts using the same stable log | Necessary recovery, but not competing authority. The difficult obligation is log-order fidelity, not a fictitious election. |
| Replacement coordinator with a shared single log and an assumed perfect fence | Can recover safe 2PC but relocates the difficult authority problem into an assumption. An unfenced replacement is not justified 2PC. Gray–Lamport explicitly identifies two supposed TMs as a correctness gap; Paxos tolerates concurrent supposed leaders.[1] |
| Two non-reused descriptors and messages left over from earlier work | Directly justified by §6.1: every message carries the transaction descriptor.[1] Adds genuine state/message separation obligations. It does not require dynamic registration or resource conflicts. |
| Arbitrarily repeated IDs, slot recycling, forgotten logs/tombstones | Interesting but distinct reclamation problem. Cannot reset the one-shot model and erase old mail, or use finite modulo IDs, while claiming repeated-transaction safety. Not necessary in this first draft. |
| Resource locks, write conflicts, deadlocks, serializability | Important database science, but not central to atomic-commit agreement. Commit does not by itself guarantee isolation. Adding these without a workload/concurrency-control source makes a second project, not recovered Gray–Lamport. |
| Registrar-selected membership | A substantive Gray–Lamport §6 extension, with a separate consensus instance choosing the participant set.[1] Defer: fixed membership already permits a substantial competing-ballot/recovery proof. |

Recovered 2PC with explicit stable/volatile logs and crash points would be a legitimate alternative if durability were the desired research question. It is not the preferred flagship because correct fixed-authority recovery ultimately refines the simple monotone skeleton, whereas coordinator replacement without external fencing requires precisely the consensus machinery supplied by Paxos Commit.

## One coherent first-draft target

### State, domains, initialization

Engineering proposal: **fixed-membership durable Paxos Commit with two transaction IDs, arbitrary participants/acceptors, concurrent ballots and retained historical messages**.

- `Tx = {T0,T1}`: exactly two globally distinct, never-reused descriptors, each including the fixed acceptor membership. Allow delayed start of T1 and overlap with T0. This supports both successive work with stale messages and overlap; it does **not** claim an unbounded number of transaction generations.
- `RM`: arbitrary nonempty finite resource-manager set. Both transactions initially use this same fixed set; different memberships are not needed to expose the central races.
- `Acceptor`: arbitrary nonempty finite set, logically distinct from RM roles. Nodes can later cohost roles, but that is not needed initially.
- `Q`: an explicit nonempty quorum family over acceptors, with every two quorums intersecting. This is the actual assumption of A.3, which does not require its safety proof to compute majority cardinalities.[1] Safety has no bound on the number of down nodes. Fault-tolerant progress would additionally require an available quorum, and is not the present theorem.
- `Ballot`: ordered nonnegative integers, including distinguished 0; each positive ballot/instance has one immutable proposal authority. Arbitrarily many ballots are a goal, not a two- or three-ballot cutoff relabeled as generality. Ballot uniqueness survives leader restart; no perfect unique-current-leader oracle.
- Each `(tx,rm)` has a durable working/prepared/committed/aborted record, volatile execution/recovery state, and persistent descriptor. Each `(tx,rm,acceptor)` has durable promise, accepted ballot and accepted value; volatile response/staging state is separately resettable. Durable terminal-event ghosts never reset.
- Sent records include transaction, instance RM, ballot, message kind, sender and value; phase1b additionally contains the immutable snapshot of the responder's accepted ballot/value at response creation. Store the full correlation, not a Yes bit independent of ballot/instance.
- Initially no transaction messages exist; active RM records are working; acceptor promises are 0 and accepted ballot is -1/value none, as in A.3.[1] Activation initializes each previously unused descriptor once. Down state is independent of protocol records; nodes may crash/recover repeatedly.

### Failure and message model

Non-Byzantine crashes lose volatile state and private unfinished construction, never durable state. Durable record writes are atomic at the chosen storage abstraction. A crash can occur before a durable write, between that write and outgoing publication, or after publication. Disk corruption, torn atomic records and Byzantine messages are outside scope. Lost publications after a successful write are recovered by retry/reconstruction.

Messages are authenticated, arbitrarily delayed/reordered/duplicated, and can remain forever undelivered. Use the paper's ever-sent history semantics as a safety overapproximation of nondelivery/loss, with receipts optional and duplicates idempotent.[1] Do not remove historical chosen evidence when current accepted state changes; `Chosen` quantifies a same-ballot/same-value quorum of historical phase2b messages. If using concrete message slots, allocation/freshness and history retention become explicit representation obligations.

Competing leaders can initiate positive ballots for the same instance at any time. A timeout does not establish failure and does not authorize arbitrary abort: the ensuing phase1 quorum determines whether a recovered value is forced. An RM durable-prepared and still waiting after recovery remains in doubt; it may query a leader, which may recover the outcome by a new ballot. No action interprets silence as permission to override a chosen value.

### Exact safety obligations

For all active descriptors `t`, participants `r,s`, ballots `b,c`, values `v,w`:

1. **Consensus agreement:** `Chosen(t,r,b,v) && Chosen(t,r,c,w) -> v=w`.
2. **Prepared origin/durability:** chosen Prepared for `(t,r)` implies that r durably prepared t and issued its unique ballot-0 Prepared proposal. A later leader cannot invent Prepared from an empty phase1 quorum.
3. **Atomic-commit agreement:** never a durable committed RM and a durable aborted RM for the same t; likewise no contradictory externally published transaction outcomes.
4. **Commit validity:** any published/installed Commit(t) has a Prepared choice for every participant, hence every participant durably prepared t. A quorum's acceptance for one RM is not a quorum certificate for all RMs.
5. **Abort justification:** a leader-published Abort(t) has an Aborted choice for some participant. Start with A.3's quorum-certified abort; do not silently add its optional ballot-0 short-circuit or mistake a positive-ballot Abort proposal for a chosen result.
6. **Stability across recovery:** a durable RM terminal state and the historical emitted outcome for a descriptor cannot be reversed. In-doubt recovery never restores working status from a durable prepared record.
7. **Descriptor/instance separation:** evidence for `(T0,r)` cannot satisfy a guard for `(T1,r)`, or for `(T0,s)`. Crash/restart cannot alias descriptor slots. This is an indexed invariant and noninterference obligation, not a comparison of two different transactions' outcomes (different transactions may legitimately decide differently).
8. **Persistence/publication contract:** every phase1b/phase2b record was generated from the durable promise/accepted update required for that response, and every ballot-0 Prepared publication has its durable RM prepare record. These auxiliary obligations connect the failure model to agreement rather than merely checking historical willingness.

The corresponding source-level theorem to pursue is refinement of the recovered protocol to the product of two `TCommit` safety specifications under projection to durable RM states, with protocol/storage/bookkeeping steps stuttering. Agreement invariants alone are useful milestones, not automatically that refinement theorem. No nonblocking, eventual response, eventual commit, starvation-freedom, or verifier-termination claim is included.

## Source/artifact action mapping and meaningful paths

A.3 has the concrete actions below; this is an actionable mapping, not a protocol-name shortlist.[1]

| Source action | Proposed model/control path |
|---|---|
| `RMPrepare(rm)` | Activate local work; branch prepare versus working-state reject. Prepare writes the stable record, returns to a crash-visible boundary, then a separate step publishes ballot-0 Prepared. Recovery republishes from durable state. |
| `RMChooseToAbort(rm)` | Durable local abort while working; publish ballot-0 Aborted. Durable prepared cannot take this path even after restart. |
| `Phase1a(bal,rm)` | Any eligible leader starts a positive ballot for the descriptor/instance; other leaders and old deliveries remain enabled between subsequent steps. |
| `Phase1b(acc)` | Receive one phase1a; increase durable promise only for a strictly higher ballot; separately publish a response containing the accepted-history snapshot. Crash/recovery preserves the promise and the response can be reconstructed/retried. |
| `Phase2a(bal,rm)` | Collect or refer to a matching phase1 quorum; compute maximum reported accepted ballot. Branch free -> Aborted, forced -> that maximum's value. Publish at most one proposal for each descriptor/instance/ballot. A.3's global no-prior-proposal guard must become durable proposal ownership/history, not an unproved oracle.[1] |
| `Phase2b(acc)` | Receive one proposal at/above promise; atomically persist promise and accepted ballot/value, then publish a matching phase2b at a separate crash-visible boundary. Ignore lower ballots; exact duplicate is idempotent. |
| `Decide` | Construct a same-instance quorum certificate; scan participant-instance certificates to derive Commit iff all Prepared, or Abort if one Aborted. Publish a descriptor-specific outcome only from complete evidence. |
| `RMRcvCommitMsg` / `RMRcvAbortMsg` | Receive certified outcome, durably install terminal state. Recovery restores it and can answer/retry. Keep a source-faithful variant without extra defensive guards to test whether authority invariants actually imply valid recipient state. |
| §6.4 learning outcome | Recovered in-doubt RM requests outcome; leader either uses retained certificate or starts higher ballots and reconstructs one. This is a distributed recovery scenario, not a single transaction path. |

### Cubicle transactions are not atomic commit — but the target need not be tiny

The distributed prepare/quorum/decision/recovery execution remains interleaved. No transaction spans remote waiting, an election, or a quorum round. That distinction does **not** force the scientific model to be a handful of trivial handlers: all coupled instance, ballot, quorum and recovery state remains in the model.

A meaningful **multi-record committed-state path** is a leader's certificate-based finalization. At entry select one descriptor; build private per-instance certificate selections from already available authenticated immutable response history; recursively visit arbitrary RM indices, validate quorum/instance/ballot agreement, accumulate outcome, and commit the decision plus participant-notification outbox only when all required certificates have been processed (or return Abort upon one certified Aborted instance). Missing evidence returns Undecided without publishing an outcome; retry happens in a later boundary action. No internal wait for a peer. Preserve the descriptor/leader bindings while changing RM/acceptor scan variables.

This is a declared transaction-level refinement candidate for A.3 `Decide`, not a claim that remote completion is atomic. A.3 already reads an existential certificate per instance and universally combines the participants in `Decide`.[1] An incremental implementation must establish that private construction commits exactly one such authorized source outcome; premature exit, wrong instance binding and omitted participant certificates remain meaningful faulty completed transactions. The scan must not be preceded by a bulk assignment that already performs the entire scan.

An optional later batching path has stronger practical provenance: §4.2 bundles acceptor responses across instances and §4.3 uses one stable-storage write for the batch.[1] If implemented, stage multiple per-instance updates privately, atomically persist the batch, then expose responses; promises, durable write and publication remain distinct crash-observable points. This optimization is **not** needed to accept the first draft and must not replace the base consensus behavior.

Cubicle's `transaction.ml:47–75,89–108,132–161` supplies entry/exit control, retained/fresh bindings, neutral-state safety and internal covering. Its exclusive trigger paths suppress other actions. Therefore crash-sensitive publish-before-persist order must be represented by ordinary boundaries, not hidden inside a path that excludes crashes. Private certificate-construction microsteps can be transaction-internal; their exit is the observation point. An explicit crash-abort exit from private construction is needed if crashes there are to be modeled directly, or a proof must show such a crash corresponds to an aborted/stuttering construction at the boundary.

For asynchronous implementation equivalence, separately prove (a) each stable/volatile action refines its A.3 action or stutter, (b) suppressed interleavings during private certificate construction cannot change the selected immutable evidence or externally observe partial state, (c) abort/crash paths expose no unauthorised output, and (d) all defining leader/message races remain between boundaries. Without that proof, label the model a committed-state protocol specification, which is a valid research target; do not market the annotations as a speedup for an equivalent fine-grained asynchronous system.

## Scenarios and negative controls the first draft must retain

Positive witnesses, not proof of liveness:

- Normal Commit with two distinct RMs and two distinct acceptors in a quorum; install at one RM while another remains prepared.
- Local RM rejection leading to certified Abort; prepared RM learns it later.
- RM crashes after durable prepare before publication; recovery republishes; and RM crashes after vote but before outcome, recovers in doubt, then learns the outcome.
- Acceptor crashes before write (no response permitted), after write/before response (reconstruction permitted), and after response (history survives restart).
- Initial Prepared proposal accepted by only part of a quorum; a higher ballot reports that history and must carry Prepared forward, or a free quorum legitimately proposes Aborted.
- An already chosen Prepared survives a replacement leader, a different intersecting quorum, and acceptor restart. Two leaders operate concurrently; neither has a perfect exclusive-leadership assumption.
- A late lower-ballot proposal is rejected after a higher promise; a positive-ballot Abort proposal is delayed while a later ballot chooses Prepared and must not itself trigger transaction Abort.
- T0 completes, retained T0 responses arrive while T1 runs; T0 and T1 can decide different outcomes without cross-contamination.
- Genuine repeated internal visits during certificate assembly for multiple instances; commit and missing-certificate return paths both reachable.

Required narrowly changed mutants/negative traces:

1. **Wrong phase1 maximum/free rule.** With acceptors A,B,C and intersecting quorums AB/BC: Prepared at ballot 0 is chosen on AB; ballot 1 collects BC, whose B response reports Prepared. If the mutant treats this as free and proposes Aborted, BC can choose Aborted, violating per-instance agreement and potentially commit/abort agreement.
2. **Forget accepted durable history.** Prepared chosen on AB; enough acceptors restart with their accepted record incorrectly cleared; a higher intersecting quorum falsely reports no accepted value, proposes Aborted, and chooses it. Crash alone without forgetting must not enable this trace.
3. **Ignore promised ballot on phase2 acceptance.** Prepared0 accepted only at B. Ballot1 phase1 quorum AC reports none and publishes Abort1, whose delivery is delayed. Ballot2 phase1 AB sees B's Prepared0, forces Prepared2 and chooses it on AB. Deliver old Abort1 to BC; the mutant permits B to accept below promise2, giving BC an Aborted1 choice. This retains a defining late-message race rather than assuming one ballot at a time.
4. **Prepared publication before durable RM prepare.** RM emits Prepared0 from volatile state; quorum chooses it and the transaction commits at another RM; the first RM crashes, restores working from its old log and rejects. Boundary agreement/durable validity fails.
5. **Proposal ownership/history forgotten.** Restarted leader emits two different values for the same descriptor/instance/ballot; two intersecting quorums can record contradictory responses. The correct model preserves one immutable proposal per ballot, including through restart.
6. **Descriptor or instance check removed.** Reuse a T0 Prepared certificate as T1 evidence although the relevant T1 RM rejected or never prepared; publish T1 Commit and install a completed conflicting outcome. A missing transaction check must not be made harmless by retaining another redundant slot-identity restriction.
7. **Premature certificate-scan completion.** Publish Commit after a strict subset of RM instances is certified Prepared while an omitted instance has certified Aborted. Fault is visible at a completed transaction boundary, not only during scratch construction.

Each proposed trace needs concrete replay against actual simultaneous updates/guards when models are eventually built. These are design acceptance scenarios, not observed executions. Keep mutants whose violations remain visible after recovery/transaction completion; do not choose only a syntactic missing-guard mutant blocked by defensive recipient checks.

## Representation needs and likely proof obstacles

- **State arity:** source `aState[instance][acceptor]` is essential. Existing `examples/ricart_abdulla_int1.cub:12–19` demonstrates two-index arrays and integer fields. For two fixed descriptors, duplicate/namescope the 2D arrays instead of presuming 3D syntax support (no 3D model example was found). Distinct logical roles need explicit tags/guards if all inhabit Cubicle's single `proc` domain.
- **Quorum relations:** encode membership, nonempty intersection and same-instance correlation; a nondeterministic Boolean `HasQuorum` is not an adequate substitute. A fixed three-acceptor development instance can illustrate traces, but is not the parameterized target. The all-finite family proof may require relational quantification beyond convenient current guards; inspect its encoding rather than quietly fixing a leader/quorum.
- **Messages and historical snapshots:** never replace all phase1b snapshots with current accepted state. Message-index process records are a possible representation, with immutable payload fields and explicit fresh allocation; their adequacy and finite-domain exhaustion need justification. Historical choice cannot be inferred solely from present accepted values.
- **Ballots and proposal uniqueness:** integer arrays exist, but maximum selection, comparisons across historical snapshots, leader-owned ballots, and restart-safe uniqueness create hard order invariants. A finite ballot abstraction must be proved counterexample-preserving/order-compatible; a capped ballot run is explicitly a bounded milestone, not completion.
- **Induction:** expect the usual relational obligations: promises dominate accepted ballots; responses reflect durable snapshots; every later proposal preserves any earlier possible choice; same-ballot proposals agree; quorum intersection transports prior choices. Then lift per-instance agreement to whole-transaction validity/stability and product-descriptor noninterference.
- **Crashes:** prove restore preserves durable authority, and private unfinished computations never become publication evidence. In-doubt states must remain reachable; excluding them to get SAFE invalidates the target.
- **Finalization recursion:** arbitrary participant and certificate scans can increase process support and located-state complexity. Convergence benefit is unmeasured. A reachable recursive path is not evidence that internal covering is necessary.
- **Source correspondence:** the paper reports finite TLC checking of its assertions, and specifically cautions that the Paxos Commit configurations are too small to detect subtle errors.[1] Printed source/theorem statements are not an already delivered unbounded Cubicle proof, nor a recovery-to-storage refinement proof.

## Acceptance and honest disposition

**Retain** the current 2PC files as controls, including their source/assumption documentation and narrowly scoped reachability/missing-vote tests. **Do not rebuild** them just to add trigger depth. **Replace** the flagship design with the target above; assess its full safety and recovery meaning before performance/annotation claims.

Completion of a future implementation requires faithful state/action correspondence, all named races reachable, replayed faulty completed traces, and a safety argument for the declared parameter domains (or a plainly recorded unresolved proof obstacle). Checker timeout/unknown is not model failure; a good ambitious model with an unfinished proof is preferable to a smaller model obtained by deleting competing leaders, crashes, history, IDs or quorum races. No runtime deadline should be used to choose a weaker protocol.

## Sources

[1] https://s2.smu.edu/~mhd/8330f11/p133-gray.pdf — Gray and Lamport: Consensus on Transaction Commit
