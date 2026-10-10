# IronKV-style repeated single-key migration

## Status and scope

This is an original protocol model informed by the IronFleet SHT contract, not a
translation or proof of the Dafny implementation. Stage 1 retains repeated
migration over the same fixed hosts, sequence-based local deduplication,
immutable replayable packet history, a receive/install separation, early ACK
cleanup, and locally authoritative GET/SET. It deliberately does not use a
current-owner oracle in protocol guards. Native resource-limit outcomes are
not safety proofs. Stage 2 is conditional on the requested SAFE/bwd versus
UNSAFE/ignore contrast and is not supplied unless that gate is met.

Sources and design mapping were read in
`../assessments/ironkv-assessment.md`, including the immutable Ironclad revision
`2fe4dcdc323b92e93f759cc3e373521366b7f691`. The previous model
`../ironkv/ironkv-safe.cub` was read as a rejected control: its global Claim/Owner
checks, once-per-host Used bit and inert ACK flag are not retained. Primary links:

- [IronFleet paper, sections 3 and 5.2](https://www.microsoft.com/en-us/research/wp-content/uploads/2015/10/ironfleet.pdf)
- [Host.i.dfy](https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/Host.i.dfy)
- [SingleDelivery.i.dfy](https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/SingleDelivery.i.dfy)
- [InvDefs.i.dfy](https://github.com/microsoft/Ironclad/blob/2fe4dcdc323b92e93f759cc3e373521366b7f691/ironfleet/src/Dafny/Distributed/Protocol/SHT/RefinementProof/InvDefs.i.dfy)

The original sources were not rebuilt or reverified. No OCaml source, checker
binary, existing family or file outside this directory was intentionally edited.

## State, transport and initialization

The proc universe is partitioned by **immutable** `Host` tags. The deployment is
all proc elements tagged True; record identities are tagged False. Host tags
are arbitrary at initialization and never updated. Thus fresh record allocation
does not create fresh hosts. In particular the same two hosts can alternate
indefinitely with unbounded separate message identities. Each finite instance
has finite record capacity and may exhaust it: the parameterized family has no
fixed cap on the non-host record supply. Infinite execution with literally
finite total memory is not claimed. Empty or one-host deployments cannot
complete the witness; the model does not assume their presence as an invariant.

`Free[m]` is a message allocator, not a host lifetime flag. Once allocated,
`Src[m], Dst[m], Number[m], Payload[m]` are immutable after publication.
`Packet[m]` never disappears. All old packets remain deliverable even after
ACK cleanup, installation, a later SET, and subsequent migrations. This is an
explicit historical nondeterministic-delivery abstraction: physical copies and
loss are quotiented into arbitrary delivery of an authentic historical record.
`Wire[d]` is one externally delivered copy waiting for receipt; `drop_wire` can
lose it. Loss cannot destroy the logical claim. The receiver has a separate
one-slot application `Inbox[d]`.

`Seq[s,d]` is the number most recently allocated by s for d, initially zero;
send allocates `Seq+1` and increments it without wrap or reuse. `Recv[d,s]` is
the receiver's accepted watermark, initially zero. Receipt requires exactly
`Number=Recv+1`. A duplicate `Number<=Recv` generates ACK only. A gap
`Number>Recv+1` is rejected. `Pending[m]` is sender-local retransmission
accounting, separate from historical packet existence. ACKs bind the immutable
endpoint pair and sequence. Cleanup clears only entries for that pair with
sequence no greater than the ACK, and never touches Held or Recv.

The one-key setting does not offer independent disjoint keys in flight. It can
still have many outstanding Pending records, because ACKs are not required
before receiver processing or later migration. Sequence gaps are guarded
explicitly even though the safe one-key protocol cannot produce two unaccepted
claim-carrying delegations simultaneously; this is not a demonstration that a
sequence gap schedule is reachable in this restricted setting.

Initialization uses a trusted once-only `provision(h)` because the attempted
conditional-root disjunctive init produced an `Init -> two Held hosts` native
trace. This initial encoding result is retained, not silently treated as a
protocol counterexample or diagnosed checker bug. The final init is conjunctive
and has an explicit extra `Provisioning` ghost claim before provision. A separate
`Initialized` setup gate prevents duplicate provision, never resets, and is
never read by migration/client guards. After setup the intended three concrete
claim kinds apply. This is a documented initialization scope change from root-
owned-at-time-zero, not an ownership oracle during the modeled protocol.

## Ghosts, properties, and local guards

`Claim`, `Witness`, `ClaimPacket` are observational instrumentation only;
`Expected` is the abstract value. `Buffers` and `Installs` are per-envelope ghost
counters. `replay.py` checks that none of these occur in any operational guard.
Only local Held/Data/Phase/inbox/watermarks, static deployment membership,
local retransmission entries and the handled message enable host actions.

The logical in-flight claim for m is derived from `Packet[m]` and
`Number[m] > Recv[Dst[m],Src[m]]`. Historical physical copies after acceptance
are not additional claims. At a boundary the bad cubes require: unique Held
host; correct authoritative, inbox and flight contents; every actual claim
consistent with the ghost's location and identity; presence of the actual
claim denoted by the witness; at-most-once buffering/installation; and no
unreceived logical claim whose Pending retransmission entry was deleted.
The witness thus encodes the no-missing direction without negating an
unbounded existential claimant relation. It is not an operational permission.
Its sound correspondence to the derived relation remains an unbounded proof
obligation when the checker does not conclude SAFE.

ACK cleanup cannot restore authority because it does not write Held. It cannot
remove a later Pending entry because its case condition includes `Number[q] <=
Number[m]`; a concrete stale-ACK/newer-Pending schedule is checked. The current
bad cube independently catches removal of an *unreceived* claim. A separate
two-state ghost certificate for preservation of already-received later Pending
entries is not encoded: that stronger per-entry frame obligation is supported
by the explicit case expression and concrete replay, not a native all-state
certificate from this run.

GET observes Data only at a local authoritative host. SET changes Data and
Expected together under the local Held check. The `redirect` step is only a
nonowner branch, not a routing-map or network redirect reply model. There are
no client identities, request/reply histories or end-to-end linearizability
claim. Two values can expose stale-data resurrection after SET but cannot
identify two equal-valued writes. Exactly-once envelope counters distinguish
repeated application independently of payload equality.

## Transaction boundaries

- **Send:** local validation and snapshot, relinquish and sequence allocation,
  then packet publication. Before publish, the ghost is InFlight but the packet
  is not yet present. This is the intended internal unsafe state.
- **Receive:** validate the delivered successor, advance Recv, buffer the claim,
  then emit its ACK. The transaction ends while the inbox still owns the claim.
- **ProcessBuffered:** install authority/data, then clear the inbox. The temporary
  double claim is private to the handler.
- **ACK:** one ordinary local atomic transition truncates sender accounting.
  It does not process or clear the remote application inbox.
- **Retransmit:** local Pending check and re-emission, represented as a stutter
  because packet history is already deliverable. It cannot resurrect a freed
  local retransmission entry. Actual `deliver` remains a separate network step.

`Phase` and `HandlerRecord/HandlerDest` are host-local private staging fields.
They make triggered transitions executable under ignored annotations **only**
after their entry established the appropriate stage and tuple. These fields
prevent the false demonstration consisting of an arbitrary standalone buffer
transition over an uninitialized packet. They do not serialize distinct hosts.
Cubicle globally suppresses interleavings inside annotated paths; a source-level
commutation/reduction theorem for that suppression is not supplied.

## Mutants and completion query

The three mutants have exactly one executable replacement each, checked by
`replay.py` after stripping comments:

1. M1 replaces the receipt successor test by positive sequence only. Old packets
   can be buffered again, even after migration and SET. It also admits gaps;
   this is the direct removal of freshness/order validation, not an oracle bug.
2. M2 adds `Recv[d,s] := 0` at sender ACK cleanup. This is an intentionally faulty
   cross-account reset. It immediately revives a historical flight claim and
   allows later stale rebuffering. The correct implementation has no such
   receiver-state write in sender cleanup.
3. M3 retains Inbox after install instead of clearing it. The completed handler
   has an installed authority and buffered claim simultaneously; processing can
   subsequently install the same envelope again.

`completion-witness.cub` has the same initialization and transitions and replaces
all bad cubes by `Sends[a]>=2 && Held[a]=True`. Expected UNSAFE is positive
reachability, not a defect. It requires a host to return after its second send,
so an A->B->A->B->A schedule suffices. It does not force intermediate destinations
to be identical in the symbolic query; the concrete replay fixes them to B.

## Independently executed adversarial schedules

`python3 tx-challenges/ironkv-migration/replay.py` executes a narrow independent
finite host-step interpreter with actual guards, fresh immutable records,
sequence comparisons and cleanup conditions. It is not a generic Cubicle parser
or an unbounded proof. It checks model/variant executable differences and guard
privacy before executing schedules. `.local/concrete-replays.json` contains
five concrete traces (including an internal before-publication control), plus
three separately decoded native counterexamples. The checked safe 21-step
schedule is:

1. A sends m1 to B; B receives, buffers and ACKs; A cleans up **before** B installs.
2. B installs and SETs One; B sends m2 to A; A receives and installs.
3. Deliver the historical Zero-valued m1 again: B recognizes a duplicate and
   does not buffer or install it. A remains authoritative with One.
4. A sends m3 to B and B installs; replay stale ACK m1. Pending[m3] remains True.
5. B sends m4 to A and A installs: A has sent twice and holds authority again.

Each mutant is also executed through a completed faulty handler, not merely
through an internal partial update. M1/M2 rebuffer and install old Zero while
Expected=One and A still holds authority; M3 completes installation with both
Held[B] and Inbox[B]. These are finite concrete evidence independent of native
checker limits. They must not be relabeled native UNSAFE verdicts if Cubicle
fails to find them within its search budget.

## Native execution results

There is **no native SAFE result**. Main bwd/all are inconclusive; the same executable
under ignore does have the intended internal-handler counterexample. All three
mutants have native UNSAFE traces ending at completed boundaries; the unchanged
completion query remains inconclusive. Stage 2 is not attempted.

All rows used GNU `timeout 120`, `-j 0 -nocolor -v`, explicit `-tx`, `-nodes`,
and `-search`; exact absolute argv is in each JSONL record. Baseline and BRAB2
rows use 1500 nodes; supplemental BRAB3/BFSh/witness recipes use 10000 nodes.
A node limit is reported after 1501 visits for a requested 1500 bound.

| Model | tx | Search | BRAB | Verdict | Exit | Seconds | Visited | Solver calls | Max proc support | Restarts |
|---|---|---|---:|---|---:|---:|---:|---:|---:|---:|
| main | bwd | bfs | - | NODE_LIMIT | 1 | 94.290 | 1501 | 220545 | 6 | 0 |
| main | bwd | bfs | 2 | TIMEOUT | 124 | 120.009 | 1098 | 326467 | 6 | 2 |
| main | all | bfs | - | NODE_LIMIT | 1 | 94.369 | 1501 | 220545 | 6 | 0 |
| main | all | bfs | 2 | TIMEOUT | 124 | 120.015 | 1095 | 325365 | 6 | 2 |
| main | ignore | bfs | - | TIMEOUT | 124 | 120.043 | 1224 | 310404 | 5 | 0 |
| main | ignore | bfs | 2 | UNSAFE | 1 | 0.501 | 18 | 1274 | 3 | 3 |
| M1 | bwd | bfs | - | NODE_LIMIT | 1 | 33.141 | 1501 | 212137 | 3 | 0 |
| M1 | bwd | bfs | 2 | TIMEOUT | 124 | 120.009 | 1093 | 328023 | 6 | 2 |
| M2 | bwd | bfs | - | UNSAFE | 1 | 31.386 | 1356 | 173641 | 3 | 0 |
| M3 | bwd | bfs | - | TIMEOUT | 124 | 120.013 | 1471 | 219344 | 6 | 0 |
| M3 | bwd | bfs | 2 | TIMEOUT | 124 | 120.011 | 1086 | 321985 | 6 | 2 |
| completion | bwd | bfs | - | NODE_LIMIT | 1 | 37.682 | 1501 | 426930 | 6 | 0 |
| completion | bwd | bfs | 2 | NODE_LIMIT | 1 | 39.598 | 1501 | 294265 | 6 | 6 |
| M1 | bwd | bfs | 3 | UNSAFE | 1 | 85.208 | 1145 | 173768 | 6 | 0 |
| M3 | bwd | bfs | 3 | TIMEOUT | 124 | 120.016 | 674 | 81995 | 6 | 0 |
| M3 | bwd | bfsh | 3 | UNSAFE | 1 | 1.741 | 258 | 9855 | 3 | 0 |
| completion | bwd | bfsh | - | TIMEOUT | 124 | 120.024 | 4665 | 2745115 | 4 | 0 |
| completion | bwd | bfsh | 6 | TIMEOUT | 124 | 120.017 | 0 | 0 | 0 | 0 |
| main | ignore | bfs | 2 | UNSAFE | 1 | 0.502 | 18 | 1274 | 3 | 3 |
| M3 | bwd | bfsh | 3 | UNSAFE | 1 | 1.768 | 258 | 9855 | 3 | 0 |

The last two rows rerun the final comment-inclusive presentation files. The
BRAB6 completion attempt timed out during forward enumeration (over 100000
reported enumerator expansions); zero backward nodes/solver calls does **not**
mean that no work was done or that the model has zero hosts. Max process
support is a symbolic-search statistic, not a host cutoff or safety theorem.

### What the native counterexamples actually establish

- **Main ignore + BRAB2:** provision; send; relinquish; bad state before publish.
  The source has relinquished, the ghost identifies a flight, but no Packet
  exists yet. This is a reachable internal prefix of the actual send handler,
  not an arbitrary standalone triggered transition.
- **M1 bwd + BRAB3 BFS:** one completed migration/install, then replay of that
  same packet and a completed second receipt/buffer/ACK. It already creates a
  host plus inbox claim and dispatches the envelope again. The native shortest
  trace does not include a later SET/return migration; the longer independently
  executed stale-Zero-after-SET trace checks that consequence separately.
- **M2 bwd BFS:** send/publish, deliver, completed receive/buffer/ACK, then
  sender cleanup before receiver install. Resetting Recv revives the logical
  flight while the inbox still claims the key. This validates both early ACK
  reachability and the completed cleanup bug.
- **M3 bwd + BRAB3 BFSh:** completed migration receipt followed by completed
  process_buffered/clear_inbox; the mutated clear leaves the inbox claim next
  to the installed authority.

All three native shortest traces were decoded and independently replayed; the
checker and concrete replay agree that every handler ends before the bad state.
See `.local/native-counterexample-replays.json` and reproduce with:

```sh
python3 tx-challenges/ironkv-migration/replay.py --native-results tx-challenges/ironkv-migration/.local/results.jsonl
```

### Reproduction and provenance

```sh
python3 tx-challenges/ironkv-migration/run.py --nodes 1500 --timeout 120
# Additional retained recipes:
python3 tx-challenges/ironkv-migration/run.py --only m1-no-dedup.cub --brab 3 --nodes 10000 --timeout 120
python3 tx-challenges/ironkv-migration/run.py --only m3-retain-inbox.cub --brab 3 --search bfsh --nodes 10000 --timeout 120
python3 tx-challenges/ironkv-migration/run.py --only completion-witness.cub --brab 0 --search bfsh --nodes 10000 --timeout 120
python3 tx-challenges/ironkv-migration/run.py --only completion-witness.cub --brab 6 --search bfsh --nodes 10000 --timeout 120
```

The default runner also tries the known successful M1/M3 BRAB3 recipes after
baseline/BRAB2 limits. Its exit zero means the recording matrix finished, not
that the model proved SAFE. Every actual checker verdict and exit is retained.

`.local/results.jsonl` aggregates the 20 final-executable protocol executions.
Original records/logs remain in `run-20261009-121745`, `run-20261009-123530`,
`run-20261009-123913`, `run-20261009-123915`, `run-20261009-124137`,
`run-20261009-124730` and `run-20261009-124731`. Presentation comments reporting
results were added after the first 18 checks. Their exact checked inputs are
archived under `.local/verified-inputs/`; `.local/input-manifest.json` records
old and presented SHA256 plus identical comment/whitespace-stripped executable
SHA256. No executable statement changed between these checks and presentation.

Earlier development attempts remain in separate `.local/run-*` directories.
They used earlier encodings and are not pooled into the final table. Incomplete
logs from interrupted development runs are explicitly classified INTERRUPTED
with unknown exit/time rather than invented counters. The earlier runner failed
to recognize the literal `Reached Limit !` marker and labeled some native node
limits ERROR_OR_LIMIT; final records correctly distinguish them.

The isolated initial-condition discrepancy is reproducible in
`.local/root-init-diagnostic.cub`, with exact argv/log and counters in
`.local/root-init-diagnostic.log` and `.local/diagnostic-results.jsonl`. It returns
UNSAFE directly from Init, zero visited nodes, two solver calls. That result is
an undiagnosed encoding/checker discrepancy, not evidence against the intended
root-owned mathematical init. It is separate from the protocol executions.

Checker SHA256: `55d4f45d494285993e01790efe84cbc6865d45bc2ac72fdac3e29e339217a2a8`. No rebuild was performed.

## Open obligations

No verified stage-1 all-round safety theorem, performance ratio, necessity of
internal covering, arbitrary-range abstraction, crash recovery, client API
refinement, or implementation-refinement result is inferred from concrete
replays or native limits. Record allocation is bounded in each concrete finite
instance, counters are unbounded integers without wrap, and fairness/liveness
is not modeled. The conditional stage-2 range work is withheld unless the
specified stage-1 native contrast succeeds. Solver resource limits and any
failed native completion/mutant obligations are reported below, not removed by
weakening the model.

## Cycle 2

### Property slicing and representation diagnostics

The first three rows are retained 600-second slice runs; the seven new controls use GNU `timeout 900`, `-j 0`, `-nodes 50000`, `-v`, explicit transaction mode and search. All commands, exits, hashes and raw counters are appended to `.local/results-cycle2.jsonl`; full logs are in `.local/cycle2-logs/`. The table uses `Number of visited nodes`, not the last printed node identifier, which can reset after a restart. Other checker processes ran concurrently, so wall times are indicative and no speed ratio is inferred.

| Variant | Mode | Verdict | Exit | Visited | Max proc support | Restarts | Wall s | Budget s |
|---|---|---|---:|---:|---:|---:|---:|---:|
| a-held | bwd / BFS / no BRAB | TIMEOUT | 124 | 7038 | 6 | 0 | 600.058 | 600 |
| a-held | bwd / BFS / BRAB2 | TIMEOUT | 124 | 5035 | 7 | 7 | 600.043 | 600 |
| b-held-data | bwd / BFS / no BRAB | TIMEOUT | 124 | 6850 | 6 | 0 | 600.038 | 600 |
| g-held | bwd / BFS / no BRAB | TIMEOUT | 124 | 8499 | 6 | 0 | 900.029 | 900 |
| g-held | bwd / BFS / BRAB2 | TIMEOUT | 124 | 5678 | 7 | 7 | 900.043 | 900 |
| h-slot-packets | bwd / BFS / no BRAB | TIMEOUT | 124 | 10799 | 4 | 0 | 900.017 | 900 |
| h-slot-packets | bwd / BFS / BRAB2 | TIMEOUT | 124 | 0 | 0 | 0 | 901.354 | 900 |
| i-slot-superseded | bwd / BFS / no BRAB | TIMEOUT | 124 | 11147 | 4 | 0 | 900.021 | 900 |
| i-slot-superseded | bwd / BFS / BRAB2 | TIMEOUT | 124 | 0 | 0 | 0 | 901.492 | 900 |
| a-held-engine-control | bwd / BFSH / BRAB2 / -candheur 2 | TIMEOUT | 124 | 6762 | 4 | 15 | 900.038 | 900 |

a is the single two-Held-host cube; b adds authoritative-data correctness. g-held copies the prepared g representation and retains only the two-Held cube; the prepared g file had all 18 cubes. h replaces fresh proc-indexed packets with current and previous slots per ordered host pair while retaining integer Seq/Recv. i combines those slots with g-style Superseded flags updated at receipt; all operational numerical guards remain. The engine control keeps the original a transition relation and adds BFSh and `-candheur 2`.

h/i are explicitly labelled diagnostic bounded-history systems, not proved reductions. Send rotates the current packet to the previous slot and discards the older packet, Pending/ACK history and counters. Wire and Inbox copy number and payload so slot rotation cannot alter an already delivered copy; acceptance requires its generation still be retained. Repeated migration, previous-generation stale delivery, early ACK cleanup, receipt/buffer/ACK before install, and host-local operational permissions remain. Dispatch counters move with slots and reset only for a new generation; their increment is placed at receipt within the same annotated receive handler. The prepared full-property adaptations expand the 18 base obligations into 32 cubes for the two retained generations and split endpoint/number identity, but are not run because the SAFE slice gate never opens. No full-history refinement theorem follows from these diagnostics.

h-slot-packets BRAB2 ends during forward enumeration: the last printed progress pair is `29904000 (4710744)`, with zero backward visits and zero solver calls. Zero maximum backward process support does not mean that the model has no hosts or that no work was done.

i-slot-superseded BRAB2 ends during forward enumeration: the last printed progress pair is `29803000 (6038087)`, with zero backward visits and zero solver calls. Zero maximum backward process support does not mean that the model has no hosts or that no work was done.

Default forward arithmetic abstraction is a source observation, not a safety theorem: `options.ml:98` initializes `abstr_num` to false; `enumerative.ml:689-725` applies `St_arith` only when that option is true and otherwise leaves the state unchanged for arithmetic actions. None of these commands selects `-abstr-num`. The mathematical slot model still permits unbounded Seq/Recv, but that fact does not explain this abstract enumerator's measured growth by itself. `-nodes 50000` bounds backward visits, not forward enumeration: the forward loop checks the separate `max_forward` option (`enumerative.ml:949`), whose default is -1 (`options.ml:94`); the external timeout is the effective forward budget here.

### Diagnosis

Bounding packet history to two slots per host pair did not restore convergence: h and i still time out in plain backward search, although maximum process support drops from 6 to 4. Their BRAB2 runs time out during forward enumeration before any backward visit, so these rows cannot be used as evidence of a backward numerical bottleneck. The g change is not an integer-state ablation: Seq/Recv and all numerical operational guards remain, and the single Held cube has no flight comparison to replace. Consequently h versus i does not remove the numerical dependencies of authority exclusion either, and this matrix cannot distinguish integers alone from their interaction with the remaining host-pair/slot representation. A source audit also shows that default BRAB ignores arithmetic updates unless -abstr-num is selected, so the forward timeout is abstract-state enumeration growth, not demonstrated enumeration of unbounded concrete sequence values. No tested representation converges SAFE; slots are not a sufficient remedy, and these diagnostic abstractions do not identify a unique cause in the original representation or open the full-property contrast gate.

### Native fault-injection variants M4/M5

| Variant | Mode | Verdict | Exit | Visited | Max proc support | Restarts | Wall s | Budget s |
|---|---|---|---:|---:|---:|---:|---:|---:|
| m4-sequence-reuse | bwd / BFS / no BRAB | TIMEOUT | 124 | 2776 | 6 | 0 | 900.016 | 900 |
| m4-sequence-reuse | bwd / BFS / BRAB3 | TIMEOUT | 124 | 1031 | 6 | - | 900.010 | 900 |
| m4-sequence-reuse | bwd / BFSH / BRAB3 | TIMEOUT | 124 | 3303 | 4 | - | 900.017 | 900 |
| m5-wrong-endpoint-ack | bwd / BFS / no BRAB | TIMEOUT | 124 | 3303 | 6 | 0 | 900.014 | 900 |
| m5-wrong-endpoint-ack | bwd / BFS / BRAB3 | TIMEOUT | 124 | 882 | 6 | - | 900.017 | 900 |
| m5-wrong-endpoint-ack | bwd / BFSH / BRAB3 | TIMEOUT | 124 | 2971 | 4 | - | 900.013 | 900 |

A dash in the restart column means the native log did not print that counter; it is not assumed to be zero.

**m4-sequence-reuse.** Not closed natively within the requested recipes and budgets; the independently executed finite concrete schedule is the only property-violation evidence.

A completes a migration to B and cleans up its ACK, resetting the sender sequence account to zero.
After ownership returns to A, a new A-to-B send reuses an already accepted sequence number.
The completed publish has no unreceived flight claim; the receiver treats the new packet as a duplicate rather than buffering it.

**m5-wrong-endpoint-ack.** Not closed natively within the requested recipes and budgets; the independently executed finite concrete schedule is the only property-violation evidence.

A completed ACK for one endpoint pair is handled while a different endpoint pair has an unreceived packet.
Cleanup compares sequence numbers without the endpoint-pair test and clears that packet's Pending entry.
The completed cleanup violates the unreceived-claim retransmission-accounting property.

In the fresh-record encoding, BRAB process count includes both hosts and non-host packet identities: BRAB2 cannot represent even one two-host send, explaining the 16-state a/g forward oracle. The retained M4 concrete schedule uses two hosts and three records (five proc elements); the retained M5 schedule uses three hosts and three records (six proc elements). BRAB3 cannot contain those particular schedules. This is a limitation of the requested finite candidate-filter population, not a bound on the backward search or a claim that no smaller M5 schedule exists.

`replay-cycle2.py` verifies the single executable replacement in each M4/M5 model and transition equality of all six a-f slices. Its four finite schedules include both unchanged controls and both fault-injection variants, recorded in `.local/cycle2-concrete-replays.json`. The separate `.local/replay-slot-diagnostics.py` executes 67 finite committed steps, including previous stale delivery, stale ACK preservation of newer Pending, early ACK before install, and 13 sends over the same two hosts (A-to-B sequence 7, B-to-A sequence 6) while retaining at most two packets per pair. This finite interpreter is not a generic Cubicle parser or an unbounded safety proof. `run.py` includes M4/M5 and uses baseline, BRAB3 BFS, then BRAB3 BFSh after a resource limit.

### Established versus open

Established: operational guards are host-local and do not read the conservation ghosts; the original full-property all-state control reaches the intended internal send prefix in 18 nodes; M1-M3 have native completed-boundary violations; M4/M5 remain concrete-only property-violation evidence. The ghost-witness cubes are not required for the observed non-convergence because authority exclusion alone already times out. Open: native SAFE on the full fresh-record representation, native SAFE even on the tested Held-only diagnostic representations, the separate contribution of numerical backward constraints versus their interaction with historical records, and any full-history correspondence for bounded slots. The bounded slots expose large two-host abstract forward enumeration rather than producing a convergent proof representation. The committed-versus-all-state demonstration on a convergent full-property representation and the conditional Stage 2 work remain unavailable.

Reproduce the requested sequential matrix with `python3 tx-challenges/ironkv-migration/.local/cycle2.py`; prepare g/h/i inputs with `python3 tx-challenges/ironkv-migration/.local/build-slot-diagnostics.py`. These retained runs used a separate g process alongside the h/i driver, so they are convergence diagnostics rather than isolated timing measurements. No OCaml edits, rebuild or commit were performed.

