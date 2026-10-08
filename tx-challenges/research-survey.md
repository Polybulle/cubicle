# Transaction benchmark candidates

## Recommendation and evidence status

Start with IronKV-style shard delegation as the new application, German incremental directory copying as the controlled recursive experiment, HIRR as the large existing workload, and JobHiring as a data-aware application. Keep current FLASH and `german_looped` as reference cases. Hemiola-inspired hierarchical MSI/MESI is the strongest longer-term coherence case because its source framework supplies serialization conditions.

This is a research shortlist, not a set of implemented or newly verified models. Three parallel investigations covered distributed-computing literature, MCMT literature/artifacts, and Cubicle examples/experiments/challenges. Parent review checked the central local models and corrected a misleading interpretation of German PFS. No verifier runs or production-code changes were made. All proposed transaction speedups remain hypotheses.

There are two distinct goals:

- Feature models: transactions define committed-state safety. An internal partial update can legitimately violate a boundary invariant. A stricter low-level model need not have the same verdict.
- Performance models: demonstrate cheaper invariant discovery/checking for the intended safety problem. Separate changing the specification from improving analysis. A reduction from an asynchronous protocol needs its own correspondence argument; alternatively state clearly that the target is its transaction-level specification.

Current `transaction.ml:47–75,89–108,132–161` supports bound arguments, cyclic call graphs, neutral-only safety, and internal located covering. Do not impose the old acyclic-path restriction on candidate selection. Conversely, recursion alone does not imply accelerated reachability or cheap internal search.

## Priority candidates

### 1. IronKV-style reliable shard delegation — new application flagship

IronFleet verifies a sharded key/value store and uses the invariant that every key is claimed by exactly one host or logical in-flight packet. Its implementation-to-host-step reduction has explicit I/O ordering obligations.[5]

Proposed scope: arbitrary hosts, one tracked key initially, symbolic values, explicit transfer identity, duplicate/reordered delivery. Extend to a fixed set of shards and then local range processing.

Safety: unique authority; no lost value/claim; accepted transfers preserve key/value; replay cannot restore obsolete ownership. Physical retransmissions represent one logical claim.

Transactions: separate sender relinquish/package/publish, receiver validate/install/acknowledge, and acknowledgment cleanup handlers. The sender-to-receiver network journey remains interleaved. This is an attractive boundary-invariant example: constructing the new ownership representation can pass through incomplete local bookkeeping before commit.

Expected value: strong realistic feature demonstration; medium encoding effort for the single-key core, substantially higher for ranges/reliability. Possible search benefit from hiding local partial updates, but no evidence yet. A parameterized local copy loop would be a distinct extension, not something established by the simple handoff.

Negative controls: omit sender relinquishment; accept stale transfer; lose the in-flight claim. Source verified code is available through the IronFleet project identified in the paper.[5]

### 2. German PFS incremental directory snapshot — clean mechanism experiment

Existing files: `examples/german_pfs.cub:67–101`, `examples/mcmt/german_pfs.in:101–202`, and data variants. German PFS appears in both early invariant-synthesis and MCMT tool evaluations.[9][10]

Important correction: the current Cubicle entry transitions already bulk-copy `Shrset` to `Invset` at lines 72 and 82. The historical MCMT input likewise assigns `l[j]` from `s[j]` on entry. The later per-index copy transitions therefore repeat an already completed copy. Do not market them as necessary recursive work.

Proposed benchmark family: (a) bulk-copy specification; (b) genuinely incremental copy under the existing isolation flag; (c) the same incremental copy as a recursive transaction. Preserve the existing ordinary invalidation, acknowledgment, and grant behavior. This is explicitly a new encoding, not merely an annotation of the current file.

Safety: an exclusive cache has no other non-invalid peer (`german_pfs.cub:44`). Copy steps change auxiliary directory state, not cache permissions. Prove finite completed copies match the bulk update and examine incomplete/nonterminating paths separately; safety equivalence is not a termination claim.

Expected value: highest interpretability for internal-loop/copy-order experiments; performance unknown. An unsafe premature grant or incomplete-copy exit should remain detectable. Check historical model licensing before redistributing derivative MCMT inputs.[16]

### 3. HIRR PV coherence — best large local workload

Existing model: `examples/challenges/hirr_pvcoherence.cub`. State includes transient L1/L2 states, forwarded requests, data replies, invalidations and UNBLOCK. Control exclusions are at lines 138–151 and data agreement at 156–163. The corpus audit located substantial retained verification workloads, but no annotated-versus-unannotated improvement.

Proposed scope: retain one symbolic line and arbitrary L1 caches. Start with exact local handlers, preserving request/owner bindings through branch-heavy updates. A larger accepted-request-to-completion transaction is a separate reduction/specification proposal; one cannot keep unrelated interleavings inside a globally exclusive transaction merely by wishing them present.

Safety: writable-copy exclusion and correct readable data; retain the existing separate property obligations. Candidate harder extensions: owner forwarding, pending invalidation acknowledgments, and eviction interactions.

Expected value: strongest ready-made serious performance workload; stronger transaction boundaries require noninterference analysis. Use full-data, abstract-data and no-data variants as representation contrasts, not independent protocol successes.

Negative control: grant exclusive permission before required acknowledgments, or forward stale data.

### 4. JobHiring receive/register/close — data-aware transaction showcase

The RAS paper explicitly protects application insertion from another insertion and supplies safe/unsafe workflow properties. The newer RAB artifact provides directly inspectable JobHiring model/property files.[7][8]

Proposed scope: arbitrary application records, distinguished workflow instance initially, finite task state, symbolic applicant identifiers. Preserve receive/register/close as a committed insertion operation only after auditing which other actions the phase permits. Protecting against another insertion is not automatically isolation from every task.

Safety: after notification each application has a valid outcome; winner references a valid non-null applicant. These are different source properties and should be separate benchmark entries.

Expected value: strong semantic showcase outside cache protocols. Richer safe RAB properties are more promising workloads than the tiny original example. A recursive row-by-row winner/outcome update can test loops, but is a new encoding of an existing bulk update and needs correspondence.

Negative controls: close with a partially initialized record; select a null applicant. Database/foreign-key abstractions are a larger encoding task than ordinary finite-state enums.

### 5. Hierarchical MSI/MESI inspired by Hemiola — strongest longer-term atomicity basis

Hemiola proves serialization under tree topology, communication-channel and locking/template conditions; its case studies include inclusive/noninclusive MSI and noninclusive MESI with evictions. Public Coq and synthesis artifacts exist.[1][3]

Proposed scope: fixed two-level topology shape and arbitrary leaves, one cache line; later fixed three levels. This does not claim arbitrary-tree parameterization just because the source supports it.

Safety: writable/readable exclusion among competing leaves and data agreement. Do not prohibit legitimate internal parent ownership using a naive global two-cache exclusion.

Transactions: request-driven flows with upgrade, invalidation responses and eviction, only under imported serialization conditions or as an explicitly transactional specification. Translation into Cubicle does not inherit the Coq theorem automatically.

Expected value: high realism, branching and changing-participant loops; high modeling effort. Better foundational support than arbitrary atomicization of FLASH flows. Performance entirely open.

### 6. Two-Phase Commit — compact distributed-commit example

Gray–Lamport gives consistency, stability and preparation validity, together with TLA+ specifications for 2PC and Paxos Commit.[6]

Proposed scope: arbitrary resource managers, one coordinator and one transaction; then recovery with durable state and multiple transaction IDs.

Safety: no committed/aborted pair; no commit without all participants prepared; terminal decisions remain stable.

Transactions: local validate/persist/publish handlers, not the whole distributed commit. Coordinator vote reception, decision and participant reception remain separate. Persist-before-send and crash observation points matter.

Expected value: accessible feature example with branching and partial local updates; basic monotone 2PC is not an especially persuasive convergence stress test. Failure blocking is liveness, not an unsafe result.

Negative controls: commit with a missing vote; send prepared before durable preparation when recovery is modeled.

### 7. Ricart–Agrawala — distributed mutex with useful local lock scopes

The original algorithm explicitly uses local semaphores around shared-variable accesses; request/reply traffic and deferred replies remain distributed.[12]

Existing representations: `examples/ricart_abdulla_int.cub`, `examples/ricart_agrawala.cub`, and MCMT inputs. Choose one encoding and exact property rather than mixing matrix-channel and process-as-message variants. The legacy MCMT input contains a suspicious `c[y]` update requiring resolution before porting.

Safety: mutual exclusion, with request-generation-correct acknowledgments. Unbounded stamps require an order-preserving abstraction if not modeled directly.

Transactions: source local semaphore regions for request handling/priority/defer decisions. A deferred-reply scan is only atomic if its actual lock scope or a reduction justifies it. Waiting for remote permissions cannot be an exclusive local transaction.

Expected value: good non-cache branch/binding benchmark; convergence uncertain. Mutations: wrong tie-break or duplicate replies counted as independent permissions.

### 8. ABD replicated register — interesting storage stretch goal

ABD's read protocol includes majority query followed by write-back to a majority before return; that second phase prevents read inversion.[11]

Proposed scope: one register, one writer, arbitrary replicas, bounded tracked operation IDs initially. Model replica tag/value compare/install/reply as a transaction; keep distributed query and write-back separate.

Safety: monotone tags, tag/value coherence, and a ghost-history no-read-inversion property. These slices are not a complete linearizability proof. Quorum intersection, ordered tags and stale responses are major encoding obligations.

Expected value: strong real-world algorithmic case, but higher effort and less direct internal-loop motivation. Negative control: remove read write-back and expose read inversion.

## Additional useful candidates and controls

- FLASH: `examples/flash_nodata_tx.cub` currently has short acyclic request/reaction bundles, not a recursive invalidation/acknowledgment transaction. Strengthen the current exclusive/exclusive-only property with exclusive/shared and data properties as separate obligations. Analyze larger flow boundaries rather than assuming they are faithful. It remains a reference forward invariant-generation workload.
- German recursive invalidation: `examples/german_looped.cub:40–48` already has a genuine changing-participant loop. Extend acknowledgment/data behavior if a heavier case is needed. Existing small SAFE runs establish functionality, not substantial speedup or necessity of covering.
- Hierarchical snoop CYGC: `examples/challenges/hierarchical_snoop_cygc.cub` has clustered/two-index state and local/global acknowledgments. Useful higher-arity binding stress after HIRR. Resolve duplicate transition name `t21` before annotation; retain intra/inter-cluster data/control properties. Full flow aggregation needs justification.
- Chandra–Toueg/send omission: `examples/chandra_toueg.cub` supplies a substantial non-cache control. Use local receive/update/send handlers; do not make the global round atomic or hide failure steps. `challenges/sendOmission_mcmt2.cub` is a comment/whitespace duplicate, not a second protocol.
- Szymanski/non-atomic bakery scans: existing matrix scans and restarts are strong search tests. Their intended non-atomicity is also precisely what blanket transactions would remove. Useful only with explicitly scoped semantics, not automatic annotation equivalence.
- Chandy–Lamport snapshots: capture local state/marker bookkeeping while normal computation continues elsewhere. FIFO ordering and marker-before-later-send matter; a globally atomic snapshot removes the interesting problem. Good interference-preservation test, expensive channel/history encoding.[13]
- Raft and Paxos Commit: rich but expensive next-stage cases. Atomic RPC/acceptor handlers are defensible; atomic elections or quorum rounds are not. Raft reconfiguration versions must not be mixed (joint consensus versus one-at-a-time membership).[6][14]
- Credit-Review-and-Approval: modern RAB workload checking accepted implies approved, with history-query subtasks. A protected task phase may provide transaction structure, but `TransactionHistory` means business data, not an atomicity theorem.[8][17]
- Timed Fischer/Lynch–Shavit, CSMA/CD and alternating-bit: useful breadth later. Preserve time elapse, collision and retransmission interleavings. These are not obvious first transaction-speedup candidates.
- Array initialization/copy/partition: mechanism controls, not distributed protocol showcases. MCMT acceleration literature supplies hard examples; quantified acceleration is a different technique and its speedups cannot be credited to transaction grouping.[15]
- Epoch lock handoff: a small replay/ownership control, not the flagship application. Separate grant from acceptance and retain in-flight states; ordered epoch abstraction must preserve stale-message behavior.

## What the MCMT literature actually benchmarks

Early invariant-synthesis tables combine mutex families (Bakery, Burns, Java M-lock, Dijkstra, Szymanski) and coherence (Berkeley, MESI/MOESI, DEC Firefly, Xerox, Illinois, Futurebus, German/PFS).[10] The MCMT tool paper adds distributed Lamport/Ricart, buggy German, imperative-array examples and timing-based mutex.[9] These are comparisons of invariant synthesis, abstraction and sometimes acceleration, not Cubicle transaction measurements.

Modern RAS/RAB adds business-process/data-aware benchmarks such as JobHiring, procurement, insurance, fulfillment and credit approval. The RAB study's specification count includes many properties per process; it must not be presented as that many independent protocols.[7][8][17]

For benchmark diversity, choose distinct protocol families and mechanisms, not every existing file. The corpus audit identifies many FLASH variants, hints-versus-properties variants and text duplicates.

## Measurement and semantic acceptance

1. Define process/data domains, initial states, bad states, failure assumptions and observation boundaries per family. Distinguish arbitrary hosts from arbitrary keys, rounds, messages or logs.
2. For a feature model, explain why committed-state safety is the intended contract, including failed paths. For an optimization comparison, establish matching boundary behavior and preservation of the property; do not infer correspondence from two SAFE outputs.
3. Include an entry-reachable meaningful recursive path and a faulty completed transaction. A no-entry loop, redundant copy, or bad-state preimage that never enters the loop is not evidence of useful recursive covering.
4. Measure explicit none/fwd/bwd/all modes, but label their semantics: backward transaction modes can change safety/interleaving; forward-only filtering is a different comparison. Do not interpret differing verdicts automatically as a tool defect.
5. Compare source-faithful ordinary, explicit isolated/locked and transaction encodings where appropriate. For copy workloads, include bulk update as a reference rather than artificially excluding the obvious efficient representation.
6. Record verdict/exit status, time, search nodes, solver calls, support, candidate rejection/restarts and available forward statistics. Use identical budgets and preserve failures. Separate fewer restarts from cheaper covering and fewer forward states.
7. To attribute convergence to internal covering, ablate only that decision in an isolated build, preserving the relation, recursive edge, boundary checks and budgets. Show backward exploration actually enters the recursive path. A bounded failure without covering is not a theorem of divergence.

Retained FLASH evidence inspected directly: `/Users/kes/LMF/cubicle-bench/flash-nodata-tx-brab2-no-depth/runs.jsonl` compares old-fwd and tetra-fwd with BRAB2, both SAFE with identical backward counters. It is an engine comparison, not transactions-on versus transactions-off, so it neither establishes nor refutes the requested transaction benefit. No new speedup was measured in this survey.

Historical MCMT distribution licensing requires author permission for modification/redistribution/derived work; check before publishing translated archive models.[16]

## Proposed first batch

- IronKV single-key delegation: new realistic semantic showcase.
- German incremental-copy triple: controlled recursion/representation experiment.
- HIRR local-handler annotation: heavy existing invariant workload.
- JobHiring insertion/outcome consistency: data-aware committed-state showcase.
- Current FLASH and German looped: reference controls, not new protocol claims.

If the first batch shows promise, add Hemiola-inspired hierarchy and Ricart–Agrawala, then ABD. Leave full Raft/Paxos Commit until the encoding effort has a clear payoff.

## Sources

[1] https://adam.chlipala.net/papers/HemiolaCAV22/HemiolaCAV22.pdf
[3] https://github.com/mit-plv/hemiola
[5] https://www.microsoft.com/en-us/research/wp-content/uploads/2015/10/ironfleet.pdf
[6] https://s2.smu.edu/~mhd/8330f11/p133-gray.pdf
[7] https://arxiv.org/pdf/1806.11459
[8] https://github.com/AlessandroGianola/RAB-verification
[9] https://homes.di.unimi.it/~ghilardi/allegati/GhiRa-IJCAR-10.pdf
[10] http://homes.di.unimi.it/~ghilardi/allegati/GhRa_tableaux09.pdf
[11] https://groups.csail.mit.edu/tds/papers/Attiya/JACM95.pdf
[12] https://www.cs.ucf.edu/~eurip/papers/Ricart-Agrawala.pdf
[13] https://lamport.azurewebsites.net/pubs/chandy.pdf
[14] https://raft.github.io/raft.pdf
[15] https://arxiv.org/pdf/1304.4499
[16] https://homes.di.unimi.it/~ghilardi/mcmt/license.html
[17] https://arxiv.org/pdf/2208.06377v2
