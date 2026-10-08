# Coherence assessment: ambition before verdicts

> Relocation note: this assessment was written before consolidation.
> `tests/transaction-models/` is now `tx-challenges/`;
> `experiments/transaction-benchmark-ideas.md` is now
> `tx-challenges/research-survey.md`. Historical model line references
> retain their numbering. Source snapshots mentioned below were research
> scratch material, not runtime dependencies; primary-source URLs remain
> in each report. No proposed replacement has been implemented here.

## Decision and ranking

**German incremental copy should remain a mechanism/representation control, not the coherence flagship. The next concrete flagship should be full-data FLASH acquisition/invalidation/acknowledgment with dirty-owner forwarding and replacement/writeback interference. HIRR remains the source-preserving hard reference and a serious second request-flow target; Hemiola-derived hierarchy with eviction is the strongest longer-term structural target.** This ranking is for the next actionable draft, not a claim that FLASH is scientifically more important than hierarchy or that HIRR is disqualified by its timeout.

1. **FLASH full-data, home + arbitrary remote caches, one line:** request-driven ownership acquisition, real invalidation and arbitrary-order acknowledgment collection, owner transfer, data conservation, and competing replacement/writeback. Ground in `examples/flash.cub`, not an enlarged no-data copy workload. Explicitly choose EAGER versus DELAYED semantics; the existing data model has an EAGER-shaped early grant. A clean-boundary transactional specification can be substantial without an already proved asynchronous reduction.
2. **HIRR full-data flow research:** retain the complete reference, its transient states, owner/requester identity and all eight queries; propose accepted GETS/GETM/UPGRADE through matching completion with stores, invalidations and eviction races explicitly represented. Its incomplete-looking branches make source completion/provenance an obligation, not a reason to erase mechanisms. No SAFE gate.
3. **Hemiola-inspired three-level noninclusive MESI with eviction:** fixed hierarchy depth, parameterized children, explicit up/down locks and request/response channels, recalls and dirty writeback. This has the strongest published serialization foundation, but importing that theorem into Cubicle still needs a translation theorem. A two-level data-bearing protocol with concurrent upgrade/invalidation and eviction is already a substantial first stage; three levels add genuine nested response collection rather than more copy work.
4. **CYGC topology/binding stress:** local/global invalidation response races and two-index cluster/cache state. Promising follow-on, but the counter abstraction and hierarchy permission semantics need auditing first.
5. **German:** controlled recursive membership-processing and cyclic-covering checks only. Its purpose is to isolate machinery; difficulty and larger counters do not make it a flagship protocol.

All proposals below are engineering specifications/research obligations. No model was changed, built or run for this assessment. No new safety, performance, refinement or serialization result is asserted.

## 1. German: genuinely executable work, but manufactured protocol difficulty

The four files were read: `tests/transaction-models/german/german-bulk.cub`, `german-incremental.cub`, `german-incremental-tx.cub`, and `german-premature-grant-mutant.cub`.

### The complement snapshot

The incremental entries do **not** bulk-copy `Invset := Shrset`; they clear `Invset` and perform the full-population update `Done[j] := if Shrset[j] then False else True` (`german-incremental.cub:122–143`; transaction variant `:124–147`). Thus entry already computes an exact Boolean membership snapshot, represented by the complement of Done. At that point `not Done[j] = Shrset_entry[j]`. Calling this “no bulk source copy” is narrowly true of the destination array, but misleading if taken to mean that snapshot discovery itself is incremental.

The later copies are nonidentity: each consumes an unprocessed member and sets its destination bit (`incremental:145–154`; tx `:149–161`). They cannot repeat that member and completion requires all Done true. Under the imposed freeze, an internal invariant is `Shrset[j] = (Invset[j] or not Done[j])`. The work is necessary **in this encoding**, because of the explicitly added completion rule. It is not evidence that the directory protocol intrinsically needs this loop: membership was already selected globally, and `german-bulk.cub:121–142` performs the useful snapshot in one array action.

This distinguishes two diagnoses: the historical PFS loop repeats a destination already bulk initialized (notes `experiments/transaction-benchmark-ideas.md:34–42`); the new loop avoids that exact redundancy, but still manufactures incremental work after a global complement snapshot. A useful control, not a substantial new coherence mechanism.

### Global freeze is stronger than “source isolation”

Flag appears on grants, invalidation sends, ack receives, requests, invalidation reception and grant reception (`incremental:103–119,156–218`; tx `:105–121,163–225`). It blocks **every ordinary protocol action**, not just writes to Shrset. Caches/channels remain unchanged throughout copy. The transaction adds CFG exclusivity to a phase already globally isolated by guards. Its requester is retained in Curptr, not a fixed recursive control binding: `copy(_)`/`finish(_)` vary participants; no owner/requester forwarding state has to survive a branch-heavy flow.

Consequently the loop does not test a snapshot racing with eviction, newly shared copies, delayed data, ownership transfer, competing upgrades, stale responses, or writeback. Those are the coherence reasons to need a serious specification.

### What the checked property misses

`unsafe(z1 z2) { Cache[z1]=Exclusive && Cache[z2]<>Invalid }` (`bulk:98`, incremental `:99`, tx `:101`, mutant `:102`) is useful permission exclusion and stronger than E/E alone. It contains no data, directory validity, pending-response conservation, request/grant association, or progress condition. During copying Cache is unchanged, so observing only committed states does not provide a new meaningful coherence abstraction for this property.

An omitted member can leave Shrset true. The correct grant requires all Shrset false (`tx:114–121`); omitted invalidation work can therefore block completion instead of reaching the bad predicate. A wrong snapshot can evade cache exclusion indefinitely through deadlock. The property does not validate `Invset=entry Shrset`, exactly-once completion, response ownership, or grant availability. The mutant switches the grant test to Invset (`mutant:115–122`): an empty *unsent invalidation* set does not mean acknowledgments completed (`:173–185`). This is a meaningful premature-grant fault control, but it mostly validates ordinary acknowledgment discipline after the copy, not the scientific necessity of copying.

Meaningful difficulty: quantified participant sets, arbitrary processing order, cyclic control and completion guards. Accidental difficulty: auxiliary support from Done/Invset, global complement/reset operations, artificial isolation and proof search over bookkeeping invisible to the property. Existing higher transaction-mode counters are not grounds to delete it, but also not evidence of greater protocol ambition.

## 2. HIRR: preserve the challenge, audit completion rather than trivialize it

The reference has exactly the same executable text as `examples/challenges/hirr_pvcoherence.cub`: this assessment independently compared both after nested OCaml-comment stripping and whitespace removal and obtained **True**. This is lexical executable identity, not a source-implementation equivalence theorem. The retained reference timeout is documented at `hirr-pvcoherence-reference.cub:89–109`; it is neither UNSAFE nor evidence the protocol is too ambitious.

Original-source mechanisms that must survive any draft:

- Arbitrary cache identifiers and data represented by proc-tag values, unconstrained Sort in initialization (`original:28–96,104–129`); adding a disjoint inhabited Proc/Data partition is a changed assumption, not a repair by default.
- Stores in M/E update CurrData (`:168–182`). A store may occur after directory acceptance but before owner forwarding; the reference gives a concrete source-guard scenario at `reference:58–67`.
- GETS, GETM and S/O upgrades, plus O/M/E eviction issue (`original:184–234`). Preserve the distinction between a requester upgrading an extant copy and one requesting data.
- Forwarded owner replies accept stable E/M/O and transient MI/OI/OM_A (`:236–392`); forwarded GETM can turn OM_A into IM_AD and needs separate data/ack handling.
- Invalidation changes IS to IS_I and SM_A/OM_A to IM_AD (`:394–445`), so data arriving after invalidation must not resurrect a readable/writable copy.
- Data-first versus upgrade-ack-first paths IM_AD→IM_A or IM_D→M (`:447–578`), old versus current PUT owner (`:885–944`), requester-specific UNBLOCK (`:946–990`). Do not collapse those into one atomic permission assignment.

**Important draft blocker to understand, not to hide:** the original is visibly incomplete-looking as a completion model. There is no stable-S invalidation handler among `:394–445`; the “NOTfinalAck” guard at `:1028–1036` has the same universal no-other-sharer test as the final SFlag branch (`:1015–1026`), rather than a some-other-sharer condition. It cannot be advertised as arbitrary nonfinal ack collection. The OTS ack branch is commented (`:1038–1045`). GETM-in-I enters IM (`:681–691`), but the memory-data handler is only for IS (`:991–1000`); memory PUTM produces MEM_ACK (`:1063–1069`) without an evident L2 MI acknowledgment receiver. These are source observations, not a proof of incorrectness or unreachability of every such state. They prohibit claiming that the retained file already supplies a complete recursive all-sharer acquisition flow.

Keep the reference unchanged. A completed HIRR-inspired specification should document each newly supplied branch separately, with recovered original protocol/artifact provenance if available; otherwise label it a new design. The lack of that provenance is an open obligation. Do not obtain SAFE by removing PUT/upgrade/interference paths.

HIRR's five permission queries (`:138–151`) cover competing stable M/E/S combinations, and its three data queries (`:156–163`) check stable M/E/S against CurrData. They do not check O data, transient-readable permissions, directory/data-in-flight conservation or read history. Whether O is externally readable must be explicitly specified before extending Readable to O; the file has stores but no load operation establishing a full memory-interface contract.

## 3. Why full-data FLASH is the most concrete next flagship

`examples/flash.cub:22–103` has home/remote state, dirty directory owner, sharer/invalidated sets, multiple message classes and CurrData/PrevData/Collecting. Its header says “without data paths”, but active data fields, stores and seven data queries are present (`:165–180`); rank by executable content rather than that header.

`flash_nodata_tx.cub:252,310,392,427` currently groups short request/reaction alternatives; it is not a recursive acquisition completion model. Two-argument owner/sharer cases at `:908–949,1039–1078` must not silently disappear when extending the flow.

The data version exposes an essential semantic choice. `flash.cub:828–870,872–915` sends PutX while invalidations are outstanding and sets Collecting/PrevData. The receiver acquires E at `:1249–1259`; invalidation and final/nonfinal acknowledgments occur separately at `:1264–1352`. Therefore naive physical E/S exclusion is not an all-state invariant of the intended EAGER-style semantics. Its existing control checks only E/E (`:161–162`), while shared data during collection is checked against PrevData, not CurrData (`:172–180`). “Strengthen to E/S and data freshness” without choosing an observation contract would inadvertently redesign the memory model.

This is not accidental weak modeling. Park–Dill explicitly distinguishes EAGER (grant before all invalidation acknowledgments) from DELAYED (grant after acknowledgments), and notes that EAGER can retain old readable copies. Their aggregation function completes already committed but unfinished operations; it does not simply forbid every competing implementation step. The paper maps implementation steps to atomic specification steps and allows compound correspondence in some branches.[5]

Talupur–Tuttle derives invariants from flows for CMP; ordering events within a flow does not establish exclusive execution.[3] The industrial extension derives constraints from interactions between flows, not merely each flow in isolation.[4] Use these sources as a design map for invariants and correspondence, not as permission to assert that the current Cubicle annotations are a proved reduction.

### Exact proposed state/contract

Initial domain: one symbolic memory line, distinguished home, arbitrary finite remote-cache population; symbolic data; reliable single-slot channel assumptions retained and stated. Do not claim arbitrary lines, arbitrary buffer depth, failures or liveness.

Maintain physical permissions, directory owner/dirty data, request identities, owner at acceptance, payload source, pending invalidation participants, and receipt/grant status. **Logical pending-response sets must represent actual protocol obligations, not just force every array cell through an artificial visit.** An invalidation loop changes permissions/messages and consumes actual acknowledgments.

Provide two separately labeled targets:

- **First transactional specification: completed acquisition, strict clean-boundary coherence.** Accepted GetX/upgrade causes owner revocation or all required peer invalidations; data may arrive before ack completion, but the committed operation exposes writable permission only after required revocations/responses. Get completes with an appropriate readable value; PutX/writeback preserves latest dirty data. This is DELAYED/completed-flow semantics, not a claim of EAGER equivalence.
- **Source-faithful EAGER/aggregation research companion:** preserve early grants and Collecting, allow old-value shared reads during the source's permitted interval, and specify abstract committed permissions/data through an aggregation map. Retain source all-state queries separately. A strict clean-boundary property must not be passed off as the original EAGER all-state safety contract.

Suggested bad predicates at a strict completed boundary:

1. Two distinct effective writable leaves, or a writable leaf and another effective readable leaf. Include home/remote and remote/remote cases separately.
2. A committed readable/writable copy differs from the logical latest value; clean memory differs when no dirty authority exists.
3. Completed acquisition with a required peer still effectively readable or an unmatched pending revocation; completion with wrong requester, owner, reply destination or payload.
4. Loss of latest value/authority: no cache, memory authority or uniquely identified in-flight transfer can account for it. Duplicate retransmission bookkeeping, if later modeled, must not count as multiple logical owners.
5. Obsolete writeback or delayed shared reply installs an earlier value/permission after a newer ownership grant.

In EAGER, use source-sensitive versions: E/E uniqueness; E data=CurrData; shared data=PrevData while Collecting and CurrData otherwise; justified abstract permission/data projection rather than literal E/S prohibition. A complete atomic-memory/sequential-consistency theorem is beyond these invariant slices.[5]

### Transactions, bindings and real recursion

The main transaction should span accepted directory request → owner forwarding or memory reply → multiple peer invalidations/acks → requester completion/directory release. It is explicitly a **multi-process committed-state specification**, not restricted to local handlers.

Bind requester r across the flow, old owner o where present, and home/directory identity. Change victim v and ack sender a on recursive edges; retain request kind and generation in state. Put/Get alternatives, home versus remote, no-sharer, one-sharer and many-sharer exits are different branches. Include equality cases as separate branch shapes where source roles can coincide: Cubicle call arguments must not be assumed to admit repeated process identities. Wildcards are selection of changing participants, not a substitute for retaining r/o. The current CFG supports cyclic successor relations (`transaction.ml:59–78`) and bound versus wildcard instantiation (`:89–108,136–152`); no acyclic-language restriction is justified.

Recursive processing must consume outstanding real obligations and preserve arbitrary response order. Accept data-first and ack-first cases, not just a deterministic visitation order. Internal state can be incoherent under the chosen commit abstraction; that is why both the boundary contract and internal conservation constraints must be stated.

### Preserve interference explicitly

At minimum retain: a shared request invalidated before its delayed Put arrives (InvMarked branches `flash.cub:1264–1281,1179–1230`); store by old dirty owner before forwarding; Get/GetX collision and Nak/Nakc recovery; eviction/writeback racing with forwarding; replacement changing directory sharer membership; home acquisition competing with remote request; grant reception before or after final acknowledgment under the selected mode; stale owner pointer and data-source alternatives; shared writeback/forward acknowledgment directory release (`:1357–1423`).

With today's globally exclusive CFG transaction, these competitors do not interleave automatically. For the transactional-specification draft, include scientifically necessary competing handlers as explicit nondeterministic internal successor families, returning to the correct acquisition phase with its r/o binding intact; allow genuine phase yields where the intended contract observes a boundary. A single global macro-flow with explicit environment choices is still not multiple independently scheduled transactions; state that restriction. Do not add a universal Flag to suppress interference. A later concurrent encoding or reduction can relate this macro-specification to the asynchronous reference.

## 4. Stages that remain substantial

**Stage A — full-data ownership acquisition specification.** Arbitrary caches, home/remote roles, genuine invalidation/ack loop, dirty-owner data forwarding, stores, delayed-reply invalidation and request collisions. Offer strict completed-flow contract plus source-faithful EAGER reference. Reachability examples must include an owner, requester, at least two revocation participants, different response orders and a later store/read-value observation. Expected mutant witnesses: completion with an outstanding ack, wrong owner data and InvMarked ignored. TIMEOUT/UNKNOWN/UNSAFE are acceptable outcomes to investigate; omission of those mechanisms is not.

**Stage B — eviction and authority continuity.** Extend the same full-data flow with dirty PutX/writeback, stale-owner acknowledgments, replacement while requests are pending, and data-in-flight conservation. Require a completed stale-writeback violation for its mutant. Keep arbitrary participant count and cyclic response collection. This is not a token extra transition: authority may move from cache to transfer to directory/memory while a requester races it.

**Stage C — hierarchy with nested recall.** Fixed three-level topology, parameterized leaves/children where encoding supports it, recursive within-cluster then cross-cluster recalls, dirty-data aggregation and eviction during upward request/downward invalidation. Use Hemiola's three logical channels and separate uplock/downlock discipline, which permits handling parent invalidations during pending upward requests; do not replace it with a global freeze.[1] Its case studies support tree-parameterized inclusive/noninclusive MSI and noninclusive MESI with arbitrary evictions.[1] Public Coq and synthesized two-/three-level MESI artifacts are available.[2] Cubicle fixed-depth topology is not arbitrary-tree verification.

CYGC supplies useful stress here (`hierarchical_snoop_cygc.cub:43–76,1058–1265`), but its Boolean sets encode numeric counter increments/decrements by choosing y, not necessarily the actual respondent c1. Mapping them to exact owed-participant sets requires justification. Its checked data properties cover Excl, not shared freshness (`:155–168`); duplicate t21 declarations at `:578,598`, update-cache data semantics (`:174–181`), and writeback guard `Clusters_RAC_Cmd=None` (`:1285–1298`) must be audited. The latter is an authentic documented interference-sensitive mutant opportunity, not evidence the current model is already faulty. Respect source-specific parent/child ownership semantics rather than banning every ancestor/descendant E pair generically.

## 5. Separate correspondence and proof obligations

A useful transactional specification is deliverable before an isolation proof. A claimed optimization/reduction is not. Keep three layers separate:

1. **Specification adequacy:** operations, allowed environment choices, committed observations, data and permission contract, abort/Nak outcomes, and no silent vacuous completion.
2. **Encoding correctness:** simultaneous RHS updates, channels/buffer abstraction, distinct binding/equality cases, sorts and data domains, participant membership, recursive call semantics and all-state versus boundary queries.
3. **Asynchronous refinement/serialization:** map source state to logical committed state; select commit steps by branch; complete in-flight effects as required; prove every ordinary step maps to stutter or permitted specification step(s), under explicit invariants. Prove source races move or map correctly rather than erasing them. Hemiola's theorem applies under its DSL topology/channel/template/lock premises, not automatically to HIRR/FLASH or a Cubicle translation.[1][5]

Open challenges: HIRR branch completion/provenance; EAGER versus DELAYED observational mismatch; nonterminating macro paths and rollback/failed completion; multiple in-flight identities versus single message slots; source sort/initialization assumptions; participant-set versus counter abstraction; nested hierarchy encoding and control-variable support; property transfer for intermediate externally observable states; internal covering/search convergence. Resource-limited failure is evidence about the checker run, not a permission to weaken the protocol. Internal-covering necessity needs a dedicated semantics-preserving ablation later, not deletion of the recursive edge.

## Scope and verification of this assessment

Read-only repository inspection included all four German models, HIRR reference and original, full-data/no-data annotated FLASH and `flash2_data` contrasts, CYGC state/properties/ack/writeback regions, transaction CFG code, and both supplied notes. The HIRR executable identity was independently checked. Primary papers/artifact were fetched; citation IDs came from a task-local ledger. No model edits, builds, verifier runs, installations or commits. The only authored deliverable is this assessment; the citation ledger is scratch metadata. A search-tool call against a fetched text failed with a process error; direct Python reading of the saved primary text provided the needed material. No conclusion depends on that failed search.

## Sources

[1] https://adam.chlipala.net/papers/HemiolaCAV22/HemiolaCAV22.pdf
[2] https://github.com/mit-plv/hemiola
[3] https://www.cs.cmu.edu/~tmurali/pubs/flows.pdf
[4] https://www.cs.cmu.edu/~tmurali/pubs/fmcad09.pdf
[5] https://dl.acm.org/doi/pdf/10.1145/237502.237573
