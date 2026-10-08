# IronKV assessment: rebuild the model, retain the problem

> Relocation note: this assessment was written before consolidation.
> `tests/transaction-models/` is now `tx-challenges/`;
> `experiments/transaction-benchmark-ideas.md` is now
> `tx-challenges/research-survey.md`. Historical model line references
> retain their numbering. Source snapshots mentioned below were research
> scratch material, not runtime dependencies; primary-source URLs remain
> in each report. No proposed replacement has been implemented here.

## Decision

**Rebuild, not incremental guard repair.** Keep the current three files only as honestly labelled local-handler feature/regression controls. They are not a satisfactory first draft of ownership migration. Keep IronKV as the application: its actual range-delegation/reliable-delivery problem is substantial enough to justify an ambitious model that Cubicle may not yet prove.

Selected first-draft target: **repeated range migration over a fixed deployment, asynchronous sequence-numbered transport with receiver buffering, and client GET/SET refinement**. Start with three ordered symbolic keys/range atoms, not just one key; parameterize hosts and message/request identities; allow arbitrarily long execution on the same hosts, including repeated A→B→A→B. Include simultaneous disjoint migrations and partially overlapping migration requests. Do not make successful migration, network delivery, acknowledgment, or a client operation one global transaction.

This is an engineering specification proposal, not an implemented/proved result. No models, checker code, options, builds, or repository files were changed or executed. Historical SAFE/UNSAFE claims in model comments were read, not freshly reproduced. The supplied parent observation that removing `Accepted=False` still gives SAFE is consistent with the independent semantic analysis below; this assessment does not claim a fresh ablation experiment.

## Evidence and scope

Local evidence: all three `tests/transaction-models/ironkv/*.cub`, `experiments/transaction-benchmark-ideas.md`, and scratch `txbench-distributed.md`. The research synthesis correctly separates specification from reduction at lines 9–14 and 138–146, but its proposed first batch at 152–158 and single-key progression at 22–28 are too weak for the corrected ambition requirement.

Primary paper: IronFleet §§3.2–3.6, 5.2.1–5.2.2. Primary artifact inspected at immutable Microsoft/Ironclad revision `2fe4dcdc323b92e93f759cc3e373521366b7f691`, obtained from the GitHub tree API. Protocol and implementation sources were fetched rather than inferred from README or filenames. Current artifact is not asserted to be byte-identical to the SOSP release, and its proofs/build were not rerun. The inspected protocol covers host operations and reliable transport.[2][3] The inspected proof/service files supply the claim definitions and client correspondence.[4][5][6] Implementation handlers were also inspected.[7]

Source snapshots are beside this report in `ironkv-sources/`. Relevant source paths below are relative to `ironfleet/src/Dafny/Distributed/` at that revision.

## What trivializes the present draft, and why

Line numbers here refer to `ironkv-safe.cub` unless explicitly stated otherwise.

1. **The supposed witness is a live global decision oracle.** `Claim`, `Owner`, `Source`, `Destination` at 122–125 are not merely ghost observations. PUT guards at 161 and 165 consult `Claim`/`Owner`; send at 169–170 consults the same global authority; receive at 184–185 requires the global current-flight tuple. A receiver is allowed to install only the one transfer the omniscient system currently designates. This removes the distributed problem of deciding from its own stale local state and the received envelope whether an old delegation may execute. Source IronKV checks a host-local delegation map for client/shard processing and local per-peer receive sequence state for delivery; `NextDelegate` does not consult a global current-transfer variable (`Host.i.dfy:97–112,129–149,165–181,185–203,265–275`).[2]

2. **One delegation per source removes repeated deployment behavior.** `Used[s]=False` at 170, permanent assignment at 176, and no reset prohibit a source's second send for its entire life. Arbitrary process count permits long chains through fresh hosts, not repeated migration on a fixed deployment. The completion query (`ironkv-completion-witness.cub:115–116`) asks for two distinct cleared sources; the documented A→B→A trace does not enable the next A→B. Source `numDelegations` is a counter with a configured bound, not a once-only source identity (`Host.i.dfy:173,194,247`); repeated sends use growing per-destination sequence numbers (`SingleDelivery.i.dfy:146–165`).[2][3]

3. **Persistent no-reuse source slots make replay vacuous as a freshness test.** The sole message identity is the sender's `proc`, with one immutable `Target[s]`/`Payload[s]` and permanent `Accepted[s]` (129–135,176,186). Messages persist, but there cannot be two generations from A whose old and new copies race at B. After acceptance `Claim` leaves `InFlight`; after another source sends, `Source` changes. The global tuple therefore already rules out reinstallation of that old slot even if the receive-side acceptance guard disappears. The acceptance bit is redundant for the hard safety problem under these restrictions, not demonstrated necessary by the existing mutant.

   **Important distinction:** immutable historical network packets are not inherently wrong. Source proofs also refer to historical sent packets; old packets cease to claim a key through the receiver's local sequence state (`InvDefs.i.dfy:166–181`). The defect is using one never-reused slot per host plus the current-flight oracle, rather than retaining immutable old identities while issuing fresh sequence-numbered transfers from the same endpoints.[4]

4. **Acknowledgments do not govern resource lifetime.** `deliver_ack` at 198–200 changes only `Ack` and `Cleared`; neither subsequent ownership action nor sequence allocation depends on either field. It frees no retransmission entry, enables no next generation, and cannot accidentally delete a later transfer. `retransmit` at 211–213 is a self-loop; physical loss is not a transition. Thus acknowledgment delivery is a completion decoration, not the actual reliable-delivery accounting problem.

   Do not overcorrect by falsely claiming ACK receipt is necessary for exclusion. Source comments explicitly say retransmission is unnecessary for safety (`Host.i.dfy:288–295`). Source ACK processing advances a per-peer watermark and truncates the unacknowledged list, and fresh sequence allocation depends on the watermark plus remaining list length (`SingleDelivery.i.dfy:57–78,146–165`). The non-negotiable safety requirement is **sound cleanup and no identity reuse**, not forced dependence of every migration on an ACK.[2][3]

5. **No client-visible history distinguishes data correctness from synchronized bookkeeping.** `Expected` is assigned together with `Data[p]` by oracle-authorized PUT at 160–166. There are no request packets, redirects, GET responses, client identities, invocation/response matching, or delayed replies. `Data[Owner]=Expected` at 152 is useful local consistency, not service refinement. Source stores processed requests and constructs replies/redirects; service correspondence checks sent replies against an abstract GET/SET service (`Host.i.dfy:41,97–163`; `AbstractService.s.dfy:19–56,109–124`; `Refinement.i.dfy:69–75`).[2][5][6]

6. **Single-key/global single-flight state removes range interactions.** `Held[proc]`, `Data[proc]` and scalar current flight cannot express two disjoint shards in transit or an attempted partially overlapping delegation from a sender that owns only part of a range. Source checks authority for the entire range, extracts its data, redirects its delegation map and removes the source data, then the destination bulk-updates the range (`Host.i.dfy:185–222,165–181`). Its comments at 188–190 record real missing-conjunct bugs found in self-recipient, membership, and whole-range authority checks.[2]

7. **The receive/install/ack split collapses a source-visible buffer interval.** Current receive triggers install then acknowledgment (183–196). Source receipt first performs reliable-delivery bookkeeping, emits an ACK and stores `receivedPacket`; later a separate `ProcessReceivedPacket` applies the delegation (`Host.i.dfy:265–285,297–306`). The source ownership invariant counts a buffered delegation as a host claim even before its local map/table is updated (`InvDefs.i.dfy:138–151,203–215`). A sender can therefore process an ACK while a receiver still holds an unprocessed delegation. Hiding this interval is not merely splitting/recombining the same source atomic handler.[2][4]

The existing negative control explicitly reactivates the remote sender during receiver install (`ironkv-unsafe.cub:192–195`); it catches fabricated double ownership, not stale-delivery or cleanup failure. Its violation is valid for that abstract model, but it does not validate freshness mechanisms. Retain as a regression control, not the flagship protocol mutation.

## Coherent substantial first-draft contract

### Domains and initialization

- A fixed deployment `H`, finite in each instance, arbitrary size across instances; hosts never appear/disappear to provide fresh transfer identities. At least a two-host deployment must support indefinitely repeated migration. Model membership explicitly and prohibit server-source spoofing.
- Exactly three ordered key positions `k0<k1<k2`. Ranges are the six nonempty contiguous subsets using four cut points. Each position has value `Absent` or a symbolic present value. This is an exact small-range problem, **not** a theorem for arbitrary ordered key spaces. Partial overlap, disjointness and multi-key movement already exist. An arbitrary-key range version is a later breadth extension, not a prerequisite for meaningful repeated ownership transfer.
- Arbitrary clients/requests and immutable message identities; per ordered endpoint pair, monotonically increasing natural sequence numbers with no wrap. Unbounded execution length, migrations, client operations, retained packet history, and outstanding sends. Two symbolic present values suffice for an explicit data-equality abstraction; retaining a tracked write identity avoids conflating equal-valued versions. No arbitrary finite cap on rounds or message generations.
- Initially every host's local delegation map points every key at a distinguished root; only the root is authoritative, and all values are absent. Sender/receiver accounts and inboxes are empty. This follows source initialization rather than adding an operational bootstrap oracle (`Host.i.dfy:75–95`).[2]

The mathematical target is defined even if the exact unbounded accounts/histories cannot immediately be encoded in Cubicle. Unsupported dimensions are unresolved encoding work, not permission to replace the target with one once-only handoff.

### Concrete protocol state

1. Per host: `delegate[h,k]` local routing/authority map and `table[h,k]`; stale nonowner routing entries are legal. No action reads another host's delegation map/table or a live global owner.
2. Per sender/destination: `acked[h,d]` and ordered `unacked[h,d]` sequence of immutable envelopes, with next sequence `acked + length(unacked) + 1`, or an equivalent monotone counter plus proved consistency. All message kinds share the same pairwise numbering discipline.
3. Per receiver/source: `recvHi[d,s]`. One host-local input buffer containing a received envelope or empty, as in the source; its contents may await processing while other hosts act.
4. Envelopes record kind, source, destination, sequence, range and complete range payload (including absent entries), or request/reply/redirect content. Identity must bind this tuple immutably. ACK envelopes bind endpoints and acknowledged sequence.
5. Sent-envelope history and a separately lossy/duplicable deliverable network, or an explicitly documented historical-message nondeterministic delivery abstraction. Retransmission is enabled only for retained unacknowledged sends. Old copies remain deliverable after cleanup; ACK cleanup never erases receiver tombstones.
6. Ghost state only: abstract table, processed operation history and reply certificates/linearization order; derived key-claim relation. A ghost owner/transfer witness may help encode conservation, but must be updated to reflect the concrete steps and **never used to enable/disable them**. Its uniqueness is an obligation, not a guard that assumes safety.

### Operations and visible interleavings

- **Receive envelope:** if inbox empty, inspect envelope using local receive account. Exactly `seq=recvHi+1` is new: advance watermark and buffer it. `seq<=recvHi` is duplicate: do not buffer; issue matching ACK. A gap `seq>recvHi+1` is not accepted; do not replace the equality test by `>` (source `SingleDelivery.i.dfy:50–55,80–113,131–137`). ACK processing updates send state independently of application processing.[3]
- **Process shard command:** validate destination membership/distinctness and *every key in the range* against the sender's local map. Reject invalid/partial-authority commands without sending a delegation. On successful enqueue, snapshot the entire payload, redirect the sender's range map and remove its table entries in the same committed host action. Allow more than one outstanding send; disjoint ranges need not wait for ACKs.
- **Process buffered delegation:** validate trusted server source, update only receiver-local map/table for the entire range and empty inbox. Do not ask whether this is the globally current migration. Dedup was performed at receipt. Buffered claim becomes installed claim without changing the abstract table.
- **GET/SET processing:** use local delegation map. If self-authoritative, read/write the local table and enqueue the identified reply; otherwise return a redirect. Client sends/retries and consumes replies separately. Delayed replies contain results of their original operations, not necessarily the latest value at delivery time.
- **Receive ACK:** only its authenticated peer/sequence may advance the corresponding watermark; remove exactly that peer's entries up through the acknowledged sequence. Later envelopes and other destinations must survive. Stale/duplicate ACKs are permitted and harmless. Fresh sequence numbers must remain increasing after truncation.
- **Retransmit/drop/duplicate/deliver:** independent network actions with arbitrary delay and ordering. Payloads cannot be corrupted or forged by server peers. No fairness is needed for safety; optional fair-delivery/host-scheduling assumptions belong only to future liveness claims.

Essential adversarial schedules: (i) A→B→A→B while first A→B packet and ACK remain replayable; (ii) B receives/ACKs but delays install while A cleans up and sends a disjoint shard; (iii) SET at B after install, B→A, then replay old A→B payload; (iv) two disjoint transfers plus a partially overlapping request whose sender lacks one key; (v) sequence n+1 overtakes n, then duplicate n, then stale ACK n after a newer pending send. These are acceptance requirements, not optional test garnish.

### Failure assumptions

Non-Byzantine hosts, trusted deployment membership, authentic sender headers, no memory corruption, no reboot that loses receive watermarks or pending/buffered claims. Arbitrary packet loss/duplication/reordering and indefinite host pauses are permitted. A paused host retains its state. Crash-recovery with persistent accounts is optional breadth: adding it later requires explicit durable-state/crash points, not silently assuming a receiver remembers tombstones after reboot. Concrete byte parsing, packet-size bounds, leases/timers, compact delegation-map implementation and arbitrary topology churn are out of scope.

## Meaningful safety obligations

1. **Derived exact claim conservation:** for every tracked key, exactly one of an installed self-authoritative host, a trusted buffered delegation, or a not-yet-received logical delegation envelope claims it. Count each immutable envelope once, not each physical copy. Include nonexistence of a missing claim as well as pairwise exclusion. Buffered claim is not yet permission to execute client operations. This is patterned on source `HostClaimsKey`, `PacketInFlight`, and `FindHashTable`, not the draft's live `Claim` oracle.[4]
2. **Content conservation/refinement:** reconstruct the abstract table from whichever installed/buffered/in-flight claimant exists. Shard, receipt, install, ACK and retransmission stutter on that table; authorized SET changes exactly its key; GET supplies the abstract value at its processing linearization point. Migration cannot resurrect an older payload after a later write.
3. **Exactly-once application dispatch:** an envelope `(s,d,n)` is buffered/processed at most once; sequence ordering is respected despite duplicates/gaps. Retaining a local watermark is essential even when its sender has cleaned up.
4. **Reliable-account integrity:** immutable sequence-to-content binding; monotone watermark/counter; unacknowledged entries have the correct destination and contiguous sequence accounting; an ACK cannot clear an unreceived/unbuffered logical claim or remove a later pending envelope. This does not claim eventual delivery.
5. **Client-visible correctness:** every successful response has a matching issued request and a certificate at an abstract GET/SET step, with no later replay causing the same request envelope to execute twice. Refine observed processed-operation/reply behavior to the centralized table service. The source abstract service checks request/reply correspondence, not merely owner/data equality.[5][6] Full end-to-end linearizability for a particular retrying client API needs invocation/completion and retry-ID semantics; do not assert it from these slices alone. The first draft must contain request/reply histories or their proved observer abstraction, not omit clients entirely.
6. **Range frame/authority:** no key outside a range changes; absent values are removed correctly; a successful shard was authorized for *all* keys at sender processing; authority for one key cannot justify overwriting adjacent ownership. Concurrent disjoint ranges stay independent.

Safety bad states are checked at every externally visible host-step/network boundary, including after receipt/ACK but before install. No single globally current in-flight shard limits concurrency.

## Transaction boundaries and source correspondence

Recommended TxCubicle boundaries match **individual source protocol steps**, not arbitrary fabricated split/recombine handlers: (A) receipt/dedup/buffer/ACK, (B) buffered shard/delegate/client processing, (C) ACK-account update, (D) retransmission enumeration. Preserve the boundary between A and B. `Host_Next` explicitly distinguishes receipt, processing and retransmission.[2] A local range-processing scan is useful only if it genuinely implements the bulk map/table operation and has an all-keys completion/frame correspondence; it is not necessary to justify selecting this problem.

Within a handler, multi-trigger paths can maintain partial private construction state, but commit the corresponding source update/output atomically. Do not include a wait for a remote ACK or a remote install. A source-level asynchronous model is the chosen target; a prescribed multi-host committed migration contract is also a legitimate *different* target, but cannot be advertised as faithful asynchronous IronKV without preserving these schedules and proving correspondence. There is no requirement to force every future useful benchmark into the local-handler form.

Mapping obligations:

| Source component | Proposed representation | Outstanding obligation |
|---|---|---|
| `NextShard` and `NextDelegate` | three-key range masks plus complete snapshots | range containment, partial overlap, deletion/absence and frame correctness |
| `SendSingleMessage`, ACK truncation | pair accounts + immutable envelope records | sequence/account invariant under repeated sends and stale cleanup |
| `ReceivePacket` then `ProcessReceivedPacket` | separate dedup/inbox and application steps | buffered claim persists despite early ACK; no duplicate rebuffering |
| `delegationMap`/hashtable | host-local per-key map/table | no global oracle; root init; stale redirect entries allowed |
| `FindHashTable`/service refinement | derived claims and ghost table/reply history | initialization and each-step simulation; value version tracking |
| implementation handlers | eventual concrete refinement, not claimed now | marshalling, bounded representation, I/O and local-step ordering |

Source implementation `HostModel.i.dfy:242–267,273–317` follows authority checks, payload extraction, successful enqueue, map/table updates and output construction, including unsuccessful-send branches.[7] Its actual bounds include max sequence, max delegations and an extracted hashtable-size threshold (`Host.i.dfy:206–222,243–247`; `SingleDelivery.i.dfy:159–165`).[2][3] The unbounded-round proposal deliberately idealizes those resource limits; finite-limit exhaustion must become a no-send/no-authority-loss branch when translating back. It is not a claim that fixed-width production counters permit infinite successful migration.

The paper's reduction is an informal argument with mechanically checked obligations, not a fully machine-checked general reduction theorem (§3.6). It requires receive-before-send ordering, preserved per-host/message order and causal delivery; time-dependent observations impose additional restrictions.[1] Translation must establish local-state privacy, those I/O constraints, endpoint/payload preservation and compatibility of the observation property with reduction. Cubicle globally suppressing unrelated steps inside a transaction does not prove those facts. The three-key abstraction also needs its own simulation before being used to claim anything about arbitrary source ranges.

## Encoding/proof risks, not reasons to weaken the target

- Distinct host/client/message roles in a shared `proc` universe need tags and explicit fresh-record allocation. Fresh immutable message identities are legitimate; fresh *hosts* as substitutes for rounds are not. Exhausting a finite number of records must not be mistaken for a theorem about indefinite migration.
- Pair-indexed receive/send accounts, sequence arithmetic, outstanding ordered lists, and message-content fields may exceed the convenient array fragment or current symbolic support. Audit the actual language/parser/SMT fragment before choosing layout. An array record per message plus source/destination fields may replace sequences only after showing exact sequence membership/truncation semantics.
- Exact conservation contains universal/existential obligations, especially no missing claimant. A witness instrumentation can assist, but using it as an operational global permission would recreate the defect. Prove observational instrumentation conservative.
- Historical reply observers must preserve request identity, value version and ordering. A finite enum of old/current/new sequence classes without proved transitions can erase precisely the replay race. Bounded concrete exploration is useful to falsify the encoding, not an all-round proof.
- Unbounded message histories and mixed integer/process domains may prevent convergence or require new invariant support. SAFE within a short budget is not acceptance. Timeout, unsupported encoding, or a plausible invariant with unproved consecution are honest outcomes.
- No acceleration/internal-covering necessity or transaction speedup is established. Most retry loops remain boundary-level; an invented recursive key loop cannot serve as evidence for the real migration problem.

## Next concrete modeling work

1. Freeze this contract and source mapping; write the mathematical transition system and invariants before selecting Cubicle layout. Explicitly list every operational guard and reject reads of global ghost ownership.
2. Inventory the existing syntax/solver support for pair accounts, integer comparison, message record allocation, range masks and quantified conservation. Return an encoding feasibility table; retain unsupported obligations rather than quietly bounding rounds.
3. Build an independent finite explorer of this *same* contract with two/three fixed hosts, three keys, several sequential messages and clients. Use it to execute the adversarial schedules above, including three-plus migrations with reused endpoints, not as the deliverable unbounded proof. This is next work, not work performed here.
4. Only then encode the asynchronous reference relation and the host-step transaction implementation. Preserve receipt/process separation and successful/failed enqueue behavior. If an internal range loop is introduced, compare its committed outcomes to the bulk specification for every processing order and incomplete path.
5. Supply narrow mechanistic negative controls: remove local dedup while keeping replay deliverable; reset receiver watermark after ACK cleanup; reuse a sequence after sender truncation; remove whole-range authority validation; delete a pending entry on an ACK for the wrong endpoint. Demonstrate concrete completed-boundary violations and report any genuinely redundant mutation. Do not use the current fabricated remote `Held[s]` assignment as freshness validation.
6. Attempt proof under fixed recorded bounds/settings without property weakening. Record source hashes, raw verdicts/limits and unresolved invariants. If Cubicle cannot express/prove the target, deliver that diagnosis and an executable substantial model—not an easier SAFE replacement.

**Why significant even without a Cubicle proof:** fixed-host repeated range transfer plus client writes forces distributed local freshness to carry correctness; sequence-number accounting, early ACK/buffering, overlapping ownership and old payloads interact across arbitrarily many generations. Those obligations are structurally absent from the current model. They connect the store's published conservation argument to the actual local transport and ownership mechanisms.[1][2][3] The artifact's claim/refinement definitions connect those mechanisms to client-visible data preservation.[4][5][6] A credible difficult first draft is useful research material and exposes verifier/encoding limitations; a quick proof of an oracle-serialized once-only handoff does neither.

## Sources

[1] https://www.microsoft.com/en-us/research/wp-content/uploads/2015/10/ironfleet.pdf
[2] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/Host.i.dfy
[3] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/SingleDelivery.i.dfy
[4] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/RefinementProof/InvDefs.i.dfy
[5] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/RefinementProof/Refinement.i.dfy
[6] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Services/SHT/AbstractService.s.dfy
[7] https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Impl/SHT/HostModel.i.dfy
