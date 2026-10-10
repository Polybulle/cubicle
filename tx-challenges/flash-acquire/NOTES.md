# FLASH acquisition experiment

## Status and scope

This is a FLASH-inspired completed-flow specification, not a verified reduction of the asynchronous FLASH artifact. The STRICT model implements a genuine request-to-release cyclic transaction. The EAGER companion preserves the executable full-data `examples/flash.cub` rules and its properties, with duplicate declaration names repaired; **it remains unannotated**, not an EAGER multi-step transaction. This is an explicit incomplete part of the requested companion design.

One memory line, arbitrary finite proc population, distinguished Home, symbolic proc-tag data, reliable single-slot channels. Home uses the same cache arrays as remote nodes, unlike the original's separate scalar fields. Data tags are not partitioned from process tags in STRICT. No failure, liveness, fairness, sequential consistency, or asynchronous refinement theorem is asserted.

## Source-rule design map

| Source `examples/flash.cub` | STRICT branch | Difference/obligation |
|---|---|---|
| PI_Remote_Get/GetX 206–271; local 220–343 | issue_Get/GetX; m/o_accept_Get/GetX_home/remote | Home equality is a distinct acceptance branch; accepted request starts Tx, issue is boundary |
| NI_Local_Get_Put 495–551 | m_reply, m_receive, m_finish_s | memory payload, delayed shared Put retained across boundary |
| NI_Local_Get_Get 476–489, GetX 635–648 | o_accept_* binds r/o | owner distinct from requester; both home and remote identities represented |
| NI_Remote_Get_Put 570–595 | o_reply, Kind=Get | owner downgraded, payload carries current owner data, shared writeback captured |
| NI_Remote_GetX_PutX 1150–1174 | o_reply, Kind=GetX | owner revoked, reply destination requester, FAck collapsed into release |
| NI_Local_GetX_PutX 828–1091 | m_accept_GetX_*; m_accept_upgrade_peer(r v) | explicit two-argument requester-already-sharer plus extra-peer shape retained; source Dir_Local True/False branches merged because Home has unified representation |
| NI_Inv 1264–1281 | m/o_inv(r [o] v) | real victim permission revocation plus ack production; InvMarked for delayed Put |
| NI_InvAck 1286–1352 | m/o_ack(r [o] v) | actual owed participant consumed, arbitrary order; final release checks empty obligations |
| NI_Remote_PutX 1249–1259 | m/o_finish_x | STRICT delays physical E until all acks consumed; EAGER original early-grant rule retained only in companion |
| NI_Remote_Put 1208–1228 | delayed_put; m/o_late_put | invalidated delayed Put cannot resurrect S |
| Store 190–202 | store; o_store(r o d) | boundary store and internal old-owner store before forwarding |
| PI_PutX/NI_Wb 348–381,1357–1364 | putx/writeback; o_evict then o_nak | writeback race can make stale directory owner abort without permission grant |
| PI_Replace/NI_Replace 386–404,1428–1454 | replace; m/o_replace | membership changes preserved; pending ack is not silently discarded |
| Nak/Nakc 410–433,438–471,1125–1145 | collision/collision_x/nak_receive; internal m/o_collision; o_nak | no permission grant on stale owner exit; directory pending represented by Phase; Nakc stored but standalone Nack-clear semantics not fully encoded |
| NI_ShWb/FAck 1369–1423 | ShWbValid/ShWbData and completion | directory update folded into completion; independent asynchronous handlers not retained in STRICT |

## Contracts and control

STRICT queries exclude E/E and E/S independently for Home/remote and remote/remote. Every physically readable copy must equal CurrData; clean memory must equal CurrData. Completed acquisition cannot retain Pending participants or required readable peers. Queries are literal all-state queries with `-tx ignore`/`fwd`, neutral-boundary queries with `-tx bwd`/`all`. Do not interpret a verdict discrepancy as performance improvement for an identical observational contract.

EAGER uses exactly the original E/E, clean-memory, exclusive-data and shared-data queries: shared copies equal PrevData while Collecting, CurrData otherwise. Early E alongside old readable S is permitted. No strict E/S property is substituted.

m_* binds requester r; o_* retains distinct requester r and old owner o throughout every recursive successor, including competitors. Victim/ack-sender v changes via `_`; store data also changes via `_`. Calls use whitespace-separated argument syntax. An owner reply itself discharges any revocation obligation for that bound owner: it revokes/downgrades the physical owner and carries the forwarded response, so the loop never needs the illegal repeated tuple `(r o o)`. Array indices must be quantified variables, not global Home: separate home properties quantify h and constrain h=Home.

At acceptance Pending/Required are snapshots of actual directory sharers needing revocation, not a manufactured all-cell visitation set. Inv handlers revoke permission and produce an ack; ack handlers consume it. Payload reply/receipt can occur before or after any invalidation/ack, preserving both data-first and ack-first paths. Grant reads UniData, not omniscient CurrData. Final guard checks actual pending obligations, not the safety predicates.

No added global freeze flag. Phase is protocol directory occupancy/flow phase, not a blanket guard on ordinary handlers. Tx CFG is globally exclusive, so relevant interference is represented by explicit internal owner-store, owner-eviction, replacement, collision and delayed-Put successors returning to m/o_step with bindings preserved. Boundary versions remain ordinary transitions. This is not independently interleaved multiple macro transactions.

## Negative and positive controls

The three generated mutants change only executable fragments of STRICT:

- premature grant: remove empty-Pending guard from both finish_x branches;
- stale owner: replace forwarded `Data[o]` payload with `PrevData`;
- ignored invalidation: acknowledge a victim without revoking its Cache permission.

Completion witness replaces only unsafe queries: requester holds E at completion and two distinct Revoked victims exist from the current flow. Expected UNSAFE means positive reachability, not a safety failure. The Revoked set is reset on every acceptance.

## Measurement

`python3 tx-challenges/flash-acquire/run.py` runs sequentially with `-j 0`, 450-second external process-group deadlines, and full logs. Recipe matrix: fwd/brab2, bwd, all/brab2, ignore/brab2. Controls use bwd/-v. Original reference uses ignore/brab3/forward-depth13, its documented finite-instance recipe. Results append to `.local/results.jsonl`; records contain exact argv, native code, wall seconds, parsed counters, hashes and log paths. Missing counters are null, not zero. Visited-node counters are the last native statistics report and may reset across BRAB restarts; they are not cumulative work. Exact recipes and search strategies are in argv. Machine was shared with other work; wall times are not isolated timing results. No executable rebuild or OCaml source edit occurred.

Initial development failures are retained rather than erased: comma-separated callee arguments caused `syntax error` at completion-witness line 144 characters 47–48; direct `Cache[Home]` query indices caused `syntax error` at strict line 57 characters 33–37. Both are fixed in generated artifacts. A completion-witness attempt was interrupted by the terminal tool at 420 seconds, before its 450-second checker watchdog; its full partial verbose log remains but no normal runner record was written. This is not classified as a verifier timeout or an UNSAFE witness.

## Open obligations

The initial executable draft also returned native UNSAFE: allowing Get from an already-S cache and Replace while a delayed Put was pending could remove directory membership and later resurrect S. Source FLASH requires Get from I and replacement only with no local outstanding command. Those source preconditions were restored, not removed as interference. For annotation-ignored semantics, memory/owner handlers now check an explicit accepted payload-source field FromOwner and OldOwner; otherwise handlers from the wrong branch were executable. An owner-bound final guard also explicitly checks Pending[o], because forall_other excludes every transition argument, including o. Earlier inputs and runs are retained; these are semantic draft repairs, not changes made to avoid a timeout. The runner refuses a batch when an input hash changes, so interrupted matrices are completed by fresh selected executions.

STRICT is a substantial specification draft, not the source protocol. Unified Home, removed sort partition, exact sharer sets replacing head/set split, collapsed ShWb/FAck and incomplete Nakc recovery all need correspondence review. EAGER companion is a renamed unannotated source reference, not the requested annotated EAGER flow. Authority-continuity properties and exact generation/message matching are not expressed; WbOwner is recorded but stale writeback rejection is not proved. A completion trace must actually establish positive reachability, not merely the existence of recursive histories. No safety or convergence claim is made without a conclusive native run.

`python3 tx-challenges/flash-acquire/replay.py` concretely executed the actual parsed guards, simultaneous RHS updates and permitted CFG successor calls over six proc tags. The STRICT 34-step trace has no bad query; the identical completion-witness trace reaches its goal with distinct victims 1 and 2, invalidations in order 1,2 and acks 2,1. Premature-grant (30 steps), ignored-invalidation (34 steps), and stale-owner (18 steps, including a store after acceptance) each reach a committed bad query. Full checked schedules are `.local/concrete-replays.json`. These concrete results are independent finite evidence, **not Cubicle UNSAFE verdicts** or an unbounded SAFE proof.

Results table is generated after executions below.

## Current-model outcomes and interpretation

For final STRICT input hash `3cc3a62336d8…`, all/brab2 returned SAFE/0 in 7.718 seconds (3,431 visited nodes, 384,806 solver calls, six inferred invariants, max five processes). fwd/brab2, bwd, and ignore/brab2 each reached the fixed 450-second deadline. This is observed convergence of the combined mode on this specification, not a measured speedup for the asynchronous source protocol or a proof that internal covering is necessary. There is no ignore-versus-bwd verdict disagreement to explain: both timed out. The observation domains still differ (all-state versus neutral boundaries).

Final premature-grant and ignored-invalidation controls returned native UNSAFE/1 at a finish_x boundary in 2.954 and 16.166 seconds. Their verbose error traces end in the actual requester grant, with a peer still readable; the ignored-invalidation trace consumes its ack first. Final stale-owner bwd timed out at 450 seconds, despite the independently checked 18-step concrete committed counterexample. Completion-witness DFS also timed out at 450 seconds; an earlier BFS attempt on a prior draft timed out. Supplemental BRAB-3 attempts also timed out (450.694 seconds for stale-owner with DFS, 450.677 seconds for completion with BFS), before backward-node exploration began; their native visited/solver counters are zero. They are separately recorded, not substituted for the default recipes. Historical UNSAFE rows on earlier mutant inputs are not accepted as evidence for the final stale-owner mutant: those drafts still contained the common replacement/branch-identity defects.

All four EAGER companion recipes timed out at 450 seconds, as did unchanged `examples/flash.cub` with its documented brab3/depth13 recipe. EAGER is unannotated, so its mode matrix is a reference workload, not a transaction optimization experiment.

## Retained execution results

All rows, including earlier failed revisions. Input SHA256 in JSONL distinguishes them.

| Model | Input hash | Mode | Outcome | Exit | Wall seconds | Visited | Forward | Solver calls | Invariants | Restarts | Max proc |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| flash-completion-witness.cub | 0a1e526021ae | bwd | error | 2 | 0.008 | — | — | — | — | — | — |
| flash-acquire-strict.cub | 5396d98aa482 | fwd / brab:2 | error | 2 | 0.009 | — | — | — | — | — | — |
| flash-acquire-strict.cub | 5396d98aa482 | bwd | error | 2 | 0.008 | — | — | — | — | — | — |
| flash-acquire-strict.cub | 5396d98aa482 | all / brab:2 | error | 2 | 0.008 | — | — | — | — | — | — |
| flash-acquire-strict.cub | 5396d98aa482 | ignore / brab:2 | error | 2 | 0.007 | — | — | — | — | — | — |
| flash-acquire-eager.cub | d09b98e2b650 | fwd / brab:2 | timeout | 1 | 450.074 | 3544 | 195947 | 7816389 | — | 3 | 5 |
| flash-acquire-eager.cub | d09b98e2b650 | bwd | timeout | 1 | 450.120 | 14684 | — | 7903765 | — | 0 | 2 |
| flash-acquire-eager.cub | d09b98e2b650 | all / brab:2 | timeout | 1 | 450.073 | 3323 | 195947 | 7518653 | — | 3 | 5 |
| flash-acquire-eager.cub | d09b98e2b650 | ignore / brab:2 | timeout | 1 | 450.076 | 3428 | 195947 | 7641477 | — | 3 | 5 |
| flash-acquire-strict.cub | 0de6d1990ab1 | fwd / brab:2 | UNSAFE | 1 | 9.083 | 756 | 742226 | 34618 | — | 0 | 2 |
| flash-acquire-strict.cub | 0de6d1990ab1 | bwd | UNSAFE | 1 | 36.904 | 10643 | — | 2320799 | — | 0 | 2 |
| flash-acquire-strict.cub | 0de6d1990ab1 | all / brab:2 | UNSAFE | 1 | 16.608 | 6219 | 742226 | 760298 | — | 0 | 2 |
| flash-premature-grant.cub | fc55e5f4e64d | bwd | UNSAFE | 1 | 6.606 | 4511 | — | 587006 | — | 0 | 2 |
| flash-stale-owner.cub | 7d0391cc62e7 | bwd | UNSAFE | 1 | 28.101 | 9648 | — | 1921587 | — | 0 | 2 |
| flash-ignored-invalidation.cub | 9f14c8dd1a43 | bwd | UNSAFE | 1 | 23.755 | 7849 | — | 1628537 | — | 0 | 2 |
| flash-completion-witness.cub | 5273b2216426 | bwd | timeout | 1 | 450.029 | 22230 | — | 13283345 | — | 0 | 3 |
| flash.cub | c059d30dbb8d | ignore / brab:3 / forward-depth:13 | timeout | 1 | 450.039 | 3585 | 62644 | 2838109 | — | 0 | 4 |
| flash-acquire-strict.cub | 3cc3a62336d8 | fwd / brab:2 | timeout | 1 | 450.035 | 652 | 323397 | 11162010 | — | 25 | 4 |
| flash-acquire-strict.cub | 3cc3a62336d8 | bwd | timeout | 1 | 450.014 | 19570 | — | 12445012 | — | 0 | 4 |
| flash-acquire-strict.cub | 3cc3a62336d8 | all / brab:2 | SAFE | 0 | 7.718 | 3431 | 323397 | 384806 | 6 | 0 | 5 |
| flash-acquire-strict.cub | 3cc3a62336d8 | ignore / brab:2 | timeout | 1 | 450.042 | 3333 | 325968 | 10676544 | — | 16 | 4 |
| flash-premature-grant.cub | 89fdadfc8225 | bwd | UNSAFE | 1 | 2.954 | 2877 | — | 309313 | — | 0 | 2 |
| flash-stale-owner.cub | 4e0d376dbf3d | bwd | timeout | 1 | 450.019 | 19573 | — | 12449760 | — | 0 | 4 |
| flash-ignored-invalidation.cub | 7fb43414fdca | bwd | UNSAFE | 1 | 16.166 | 5533 | — | 1167515 | — | 0 | 2 |
| flash-completion-witness.cub | 079d208175e6 | bwd / search:dfs | timeout | 1 | 450.024 | 10100 | — | 9467633 | — | 0 | 7 |
| flash-stale-owner.cub | 4e0d376dbf3d | bwd / brab:3 / search:dfs | timeout | 1 | 450.694 | 0 | — | 0 | — | 0 | 0 |
| flash-completion-witness.cub | 079d208175e6 | bwd / brab:3 | timeout | 1 | 450.677 | 0 | — | 0 | — | 0 | 0 |
