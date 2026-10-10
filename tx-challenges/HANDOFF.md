# Handoff: FLASH acquisition and IronKV migration showcase models

Session of 2026-10-09, Tetra with Kes. Nothing committed. Working tree has two
new directories under `tx-challenges/` plus review logs under
`tx-challenges/.local/review/` (gitignored). Read this before `REPORT.md`; it
supersedes the status statements there for the two new families.

## Decision taken

Of the five existing families, only IronKV and full-data FLASH were worth
rebuilding as showcases: both have real provenance, and in both the committed-
state reading is the intended contract (`-tx ignore` rejects the models at an
internal handler state while `-tx bwd` accepts them). German is a mechanism
control, 2PC and the old JobHiring exhibit nothing under `-tx ignore`, HIRR has
no justified transaction grouping. Paxos Commit remains a later stretch target.

Both rebuilds were done by GPT-6.1 builders under design briefs; Tetra reviewed
at design level and ran the confirming checks listed below.

## FLASH: `tx-challenges/flash-acquire/` — complete, showcase-ready pending cleanup

Model: `flash-acquire-strict.cub`, one line, Home plus arbitrary remote caches,
symbolic data. One transaction = one request flow: entry at directory acceptance
(Get/GetX, home/remote requester, clean memory or dirty owner: eight entries plus
a peer-upgrade case), hub step `m_step(r)` / `o_step(r o)`, branches for memory
reply, owner reply, recursive invalidate/ack with changing victim `_`, data-first
and ack-first, stores/evictions/replacements/collisions/late Puts as explicit
internal successors (no freeze flag), exit at grant/release. Strict (DELAYED)
contract: E/E, E vs readable (home and remote cases), readable copy = CurrData,
clean memory = CurrData, no pending ack / required readable peer at completion.
`flash-acquire-eager.cub` is an unannotated source-faithful reference only.

Results (all on this checker, `-j 0`; wall times for the long runs were taken
with other checkers running and must be re-measured sequentially before quoting):

    strict  -tx all -brab 2                SAFE     7.7 s   3431 nodes,  6 invariants,  0 restarts
    strict  -tx ignore -brab 2             SAFE  ~1100 s    440 nodes, 60 invariants, 29 restarts  (.local/review/strict-ignore-brab2-1800.log)
    strict  -tx fwd -brab 2                SAFE   1310 s    451 nodes, 61 invariants, 45 restarts  (.local/review/strict-fwd-brab2-1800.log, /usr/bin/time)
    strict  -tx bwd (no brab)              timeout 450 s (builder run; not retried with a longer budget)
    premature-grant mutant   -tx bwd -v    UNSAFE   3 s     committed grant with pending acks
    ignored-invalidation     -tx bwd -v    UNSAFE  16 s     committed grant with a sharer readable
    stale-owner              -tx all -brab 2 -v  UNSAFE ~60 s  o_finish_x with stale data (.local/review/flash-stale-owner-all-brab2.log)
    witness, one sharer      -tx all -brab 2 -v  reachable, 1429 nodes (.local/review/witness-one-sharer*.log; file is a review copy)
    witness, two sharers     -tx all -brab 3 -forward-depth 45 -v  reachable, ~19 min (.local/review/witness-all-brab3-d45.log)

Reading: fwd and ignore behave alike (sixty-odd invariants, dozens of restarts)
because both check the all-state property; the gain lives entirely in checking
the committed-state property (bwd), and the forward filter then keeps candidates
that are cheap to prove at boundaries. The honest sentence is "the committed-
state specification is provable in seconds; the all-state one holds too but
costs ~20 minutes", not "same problem, 140x faster". The two-sharer witness
needs three-process forward exploration because the goal has three processes.

Remaining work: (1) sequential re-measurement of ignore/fwd/all and a longer
bwd-only run, folded into the builder's `NOTES.md` table together with the
review runs above; (2) decide whether to annotate the EAGER companion — Tetra's
inclination is no: EAGER grant-before-acks is precisely what boundary-only
safety cannot honestly claim, so keep it as the baseline; (3) consider an
InvMarked mutant (delayed Put resurrecting a shared copy) as a fourth control.

## IronKV: `tx-challenges/ironkv-migration/` — sound model, no native SAFE

Model: `ironkv-migration.cub`. Fixed host deployment (Host[] tag), repeated
A->B->A->B migration, per-pair integer Seq/Recv with exact-successor receipt,
duplicate ACK-only, gap rejection; receipt/buffer/ack path separate from
install; immutable proc-indexed packet records that stay replayable after
cleanup; cleanup truncates Pending by endpoint pair and sequence. Every guard
reads only the acting host's state and the handled packet; ghost
Claim/Witness/ClaimPacket are written, never read in guards. Fifteen unsafe
cubes (exclusion, data, conservation via ghosts, at-most-once counters,
pending coverage).

Results:

    main   -tx ignore -brab 2      UNSAFE in 18 nodes at send/relinquish before publish (intended contrast)
    main   -tx bwd / all -brab 2   timeout 1800 s, ~1583 nodes, 6 restarts, identical counters in both modes
    m1 no-dedup                    UNSAFE bwd brab3, 85 s      m2 reset-watermark  UNSAFE bwd, 31 s
    m3 retain-inbox                UNSAFE bwd brab3 bfsh, 1.8 s
    m4 sequence-reuse, m5 wrong-endpoint-ack: concrete replays violate committed boundaries; no native close at 900 s (bwd, brab3, bfsh)
    completion witness             inconclusive natively; finite replay reaches it

Cycle-2 ablation, single "two Held hosts" cube only (`.local/slices/`,
`.local/results-cycle2.jsonl`, NOTES.md "Cycle 2"):

    a original                     bwd timeout 600 s, 7038 nodes, support 6
    g Boolean superseded flag      bwd timeout 900 s, 8499 nodes, support 6   (no-op: Seq/Recv still in all guards)
    h two packet slots per pair    bwd timeout 900 s, 10799 nodes, support 4
    i h + g                        bwd timeout 900 s, 11147 nodes, support 4
    BRAB on h/i                    timeout in forward enumeration before any backward node (default BRAB ignores arithmetic updates without -abstr-num)

Diagnosis: the obstacle is the relational ordering invariant that makes
exclusion inductive ("the packet with Number > Recv[d,s] is the unique claimant;
every packet with Number <= Recv is consumed"), i.e. IronFleet's
HostClaimsKey/PacketInFlight invariant, which Dafny had by hand. Neither
bounding messages nor removing one comparison helps; transactions neither help
nor hurt (bwd = all).

Note for the engine: a disjunctive init `(h = Root && Held[h] = True) ||
(h <> Root && Held[h] = False)` yields "Init -> two Held hosts" and reproduces
with `-tx none`, so it is baseline Cubicle's init handling
(`.local/root-init-diagnostic.cub`). Worth a look at `typing.ml` init_cdnf.

Options on the table (Tetra's recommendation: do 1 and 2 in one short cycle,
then 3 regardless):
1. Supply the ordering invariant as a trusted boundary `invariant` (its
   intended use in `-tx bwd`) and see if the rest closes: "proved modulo one
   hand-supplied invariant", as IronFleet states it.
2. Try `-abstr-num` on the slot variants so BRAB's forward phase can finish.
3. Keep IronKV as the suite's open hard case; put showcase weight on FLASH.

## Housekeeping

- Builders were told not to commit; `git status` shows the two new directories
  and no changes elsewhere. `tx-challenges/README.md` and `REPORT.md` do not
  yet mention them.
- One builder run was killed by the provider's content filter on verification
  vocabulary ("mutant", "adversarial schedule"); briefs for this suite should
  say "fault-injection variant" and "interleaving".
- `tx-challenges/.local/review/` holds Tetra's confirming logs; `witness-one-
  sharer.cub` there is a review-only copy of the witness with a one-sharer goal.
