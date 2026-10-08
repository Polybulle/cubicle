# JobHiring assessment: rebuild the relational workflow, retain the current files only as regressions

> Relocation note: this assessment was written before consolidation.
> `tests/transaction-models/` is now `tx-challenges/`;
> `experiments/transaction-benchmark-ideas.md` is now
> `tx-challenges/research-survey.md`. Historical model line references
> retain their numbering. Source snapshots mentioned below were research
> scratch material, not runtime dependencies; primary-source URLs remain
> in each report. No proposed replacement has been implemented here.

## Decision

**REBUILD JobHiring, rather than abandon the source or promote the current draft.** The source has substantial mechanisms that the draft removed: two evolving relations, independent business identities, competence joins, duplicate-conflict replacement, deadline closure and threshold-based bulk results. RAB additionally supplies selection, withdrawal, deletion/reuse and universal insertion guards. These are enough for a significant first draft without inventing distributed messaging, crashes, ranked single-winner elections or repeated hiring rounds.[1][3]

Keep the three existing files as transaction-syntax/negative-control/completion regressions. They are not a persuasive data-aware benchmark. No requirement here is that a rebuilt model prove SAFE or converge immediately. This assessment changes no `.cub` file, runs no verifier and makes no performance claim.

## Evidence examined

- All three executable models in `/Users/kes/cubicle/tests/transaction-models/jobhiring/`, not just their presentations.
- Downloaded primary RAS and RAB PDFs and extracted them with `pdftotext -layout`; retained under `txmodels-ambition/sources/{ras,rab}.{pdf,txt}`.
- Actual RAB `E+P17.txt`, retained in that same directory. Its transition values are positional; the field order is declarations at lines 53–87.[3]
- Local `parser.mly:182–185` and `transaction.ml:47–75,89–108,132–161` for concrete language/control evidence.
- Git status was already dirty. No build, model edit, commit or repository write was performed. Root `AGENTS.md` was absent. PyMuPDF was unavailable; the installed `pdftotext` provided the primary-paper text instead.

## What the source actually says

### RAS: not a one-slot/one-person election

The paper's Example 3.1 describes separate UserId, EmpId, JobCatId and CompInId domains, with `who: CompInId -> EmpId`, `what: CompInId -> JobCatId`, and naming/description functions. Artifact row indices are separate from those business identities. Competence is an actual join: receipt chooses a competence record whose employee and category match the pending application. See PDF Example 3.1 and Example 4.1, extracted `ras.txt:286–296,615–649`.[1]

**Winning means score > 80, not highest score, and not unique winner.** Example 4.1 and Appendix A.1 simultaneously classify every application. Multiple winners are legitimate; a zero-winner completed notification is also legitimate. Example 4.2's property only forbids an occupied row having neither winner nor loser after notification; it does not establish score correctness or uniqueness. See `ras.txt:659–694,1406–1418`.[1]

Appendix A.1 is more substantial than the small main-text example: multiple job offers and applications co-evolve. Offers carry category/date/state; publication replaces any old row for the same category. Application insertion replaces conflicting old rows; assessed scores are initially -1 and subsequently 0..100. A nondeterministic deadline moves Enabled to Final and closes all open offers; Final to Notified computes outcomes. See PDF pp. 22–26, extracted `ras.txt:1164–1203,1216–1284,1285–1418`.[1]

**Do not silently resolve paper discrepancies.** Appendix A.1's prose says duplicate applications are excluded for the same user/category, but the displayed replacement condition uses `(applicant[j]=uId && appResp[j]=eId)`, i.e. user/employee (`ras.txt:1330–1379`). Its job-offer-selection display both copies `joCat[i]` into `jId'` and resets `jId'` to undef (`1310–1314`). These are genuine source specification questions, not permission to normalize the equations. A first draft must explicitly pick the literal relation or document a repaired relation and its intended correspondence. The main text's multiset variant deliberately allows repeated tuples; Appendix A.1 observes that dropping conflict handling changes locality/termination considerations (`1420–1427`). Removing duplicates is not an innocuous convenience for proof.[1]

### RAB artifact: useful lifecycle, but not arbitrary simultaneous tasks

`E+P17.txt:12–29,53–87` retains separate DB sorts, competence functions, JobOffers and Application fields, and three tasks' working variables. ReceiveApplication's guard joins `who(e)=g` and `what(e)=T2_jid` and requires a non-null user (`684–724`). RegisterApplication picks an empty row, writes user/category/employee together and sets score -1 (`726–802`). The universal guard at line 732 lists three separate inequalities. It must not be paraphrased as merely “no identical full tuple”; inspect MCMT's conjunction semantics before translating it.[3]

HiringProcess includes job creation/publication/selection, timeout and assessment (`112–478`). DetermineWinner writes Winner for every row whose score >80, else Loser, and marks Notified (`480–557`). The checked P17 formula is precisely `Winner=Application5[z1] && Application1[z1]=T1_uid && T1_uid=NULL_UserId` (line 96); it differs from the notification-totality query.[3]

Withdraw is real: select an application and copy its user/employee/category/score into working memory (`560–637`), open task T3 (`849–891`), locate a row matching all four values, clear all five Application columns to null/-1, then close (`893–1010`). Thus deletion frees storage for later insertion; row identity and business identity can diverge over time.[3]

However, the inspected artifact **serializes its task executions**: T1 actions require actT2=false and actT3=false; T2 requires actT2=true/actT3=false; T3 the reverse. Preserve alternative task scheduling and lifecycle choices at boundaries, not fictitious overlapping T2/T3 transactions. RAS Appendix A.1 has distinct publishing/insertion control; its assessment guard does not require aState=undef (`ras.txt:1390–1393`). Treat that source separately: neither blanket exclusion nor arbitrary concurrency transfers automatically.[1][3]

RAB introduces arithmetic and universal guards for relational actions over a static database and unbounded mutable working relations. Its approximate backward search guarantees correct SAFE, but UNSAFE can be spurious when universal guards are used; those meta-results do not transfer to a Cubicle encoding without a separate argument. See RAB §§2–5.[2]

## Why the current draft is superficial

Executable references below are to `jobhiring-safe.cub` unless stated otherwise.

1. `Applicant[i]=i` at initialization (125–129), and every subsequent Applicant write also assigns i (160–174). Therefore the alleged identity check at 134 cannot fail even internally. It is a construction invariant with no relational join or identity propagation problem.
2. `Qualification` is a chosen enum, not an assessment stored for a user/category/employee tuple. No offers, competence relation, score threshold, publication replacement or deadline remain.
3. `select(w)` (186–194) chooses any eligible row, assigns a global Winner, and never returns to Intake. All later writes to Result are Lost. Unique winner is guaranteed by this one-shot control structure, but conflicts with the sources' legitimate multiple-winner classification.
4. `notify(w)` (205–211) directly demands winner Won and every other published row Lost. This almost restates the notification-totality query (146); it checks that callers did not skip the completion gate, rather than deriving correct results from assessment data. The recursive loser loop is genuinely incremental, but its semantic payload is only enum completion.
5. Publishing a received row early (151–155) manufactures intermediate inconsistency against 131–133. The parent's reported experiment moving Published to commit and obtaining ordinary-ignore SAFE is consistent with this being a visibility-placement demonstration, not evidence that the source workflow needs this transaction. I did not rerun that experiment.
6. No deletion, overwriting or slot reuse exists. Row indices act as immutable applicant identities; intake repeats only by consuming never-used cells. Finished is terminal.
7. `jobhiring-unsafe-notify.cub:204–209` removes just the completion scan: a useful completed-boundary negative control, but an easy scheduling fault, not evidence for the missing relational mechanisms. `jobhiring-notification-reachability.cub:130–132` checks three completed rows; that establishes reachable loop work, not a hard invariant or necessity of internal covering.

Committed-state specifications are legitimate. None of these criticisms requires checking every internal state. The issue is lack of substance in the state, relation and boundary property, not the use of transactions itself.

## Significant replacement first draft: relational publication and application conflict replacement

**Recommended core: RAS Appendix A.1 transactional specification, retaining its cross-record replacement.** Keep a separate RAB lifecycle extension; do not splice incompatible source guards without declaration.

### Verification tuple and parameterization

- All finite populations of offer rows O and application rows A, and independent finite User, Employee, Category and Competence domains; no fixed row count. The intended symbolic claim quantifies over all such populations and every admissible static competence database.
- Static total functions `who(C)` and `what(C)` plus domain-validity/null predicates. Category/date and applicant/responsible/category are stored, not inferred from row indices. Business identities may be reused in a different row; different rows may temporarily represent the same business key during an operation.
- Mutable Offers = (category,date,state), Applications = (user,category,responsible,score,result). Empty rows have null fields and score -1. Initial process Disabled; relations empty; working registers null. Use Enabled, Final, Notified and explicit pending-operation control.
- RAS score domain really is finite -1..100. An optional threshold quotient {Unassessed, AssessedAtMost80, AssessedAbove80} is defensible only if this model uses no other score comparisons or score identity matching. For RAB withdrawal's exact score match, retain full score data or prove a different abstraction.
- No invented resets, ranked election, crashes or remote delivery. Re-publication/replacement and later row reuse are already source-relevant. Full lifecycle withdrawal belongs to the separately declared RAB variant.

### Operations and transaction boundaries

1. **Publish(category,date,newRow)**: load pending category/date; write new offer fields; recursively find and clear old offers of the same category, excluding newRow; commit publication. This decomposes the source's bulk replacement, not an already supplied source loop.
2. **Apply(user,offer,competence,newRow)**: select an open offer; validate `what(competence)=offer.category`; obtain `responsible=who(competence)`; store independent user/category/responsible; recursively clear conflicts; commit and reset pending registers. Bind the selected category/user/competence/newRow across calls while changing scanned participants.
3. **Assess(row,score)**: at a permitted boundary, change -1 to 0..100; retain source control guards. No result is chosen nondeterministically.
4. **Deadline**: when source publishing/application control permits, move Enabled to Final, recursively close open offers, commit. Scores thereafter cannot change.
5. **ClassifyAndNotify**: from Final, recursively assign each row's result from its frozen score; commit Notified. Completion checks structural coverage (no unprocessed occupied row), **not** that every stored outcome is already the right outcome. This makes correctness of the loop body independently necessary.

For conflict key choose one explicitly: **literal-paper user/responsible**, or **prose-intended user/category repair**. Prefer the literal key for the first executable artifact, displaying the prose discrepancy. Do not claim user/category uniqueness if the model only implements user/responsible replacement. Offer uniqueness by category is unambiguous.

In the target transactional specification, these service operations are exclusive, including arbitrary numbers of affected records. Ordinary source task actions remain choices at committed boundaries. This is a declared macro relation, not an automatic annotation equivalence: prove each completed recursive operation equals the chosen source bulk update; analyze which original partial-operation interleavings are suppressed. RAS assessment during receipt may commute only for untouched rows; application replacement can delete the very row assessed, so a blanket commutativity assertion is not justified.

### Actual committed-state invariant

At every neutral boundary:

- No two occupied offer rows have equal category.
- No two occupied application rows share the **declared** application conflict key.
- Every occupied application has valid independent user/category/responsible data and a competence witness linking its responsible employee to its category. A stored witness may make this existential dependency executable, but preservation of its `who/what` equalities must be checked, not assumed from row identity.
- Final or Notified implies no occupied offer remains Open.
- Notified implies, for every occupied application, `(result=Winner iff score>80)` and `(result=Loser iff score<=80)`. Multiple winners and no winners are permitted.

Do not assert that an application permanently references the same physical offer row: publication moves/replaces offer rows, and the source application stores a category, not a row FK. Do not assert every application is assessed before completion: unassessed -1 legitimately loses.

### Why this is interesting

Uniqueness is no longer baked into identity or guarded by a “property is true” exit. Inserting a fresh row can temporarily duplicate a business key, and recursive cleanup must erase precisely the old matching records while preserving unrelated categories/users and all coupled columns. Re-publication and insertion repeatedly reclaim rows. Competence must survive working-register transfer, cleanup and subsequent reuse. Notification correctness follows frozen data and correct per-record computation, not just absence of Pending. A mutant using the wrong conflict key, clearing only some columns, corrupting responsible/category, or applying the wrong threshold can violate a completed-state invariant despite complete scans.

The main proof challenge is coupling an unbounded destructive scan with preservation/frame conditions and business identity aliasing, then composing publication, insertion, assessment and closure. Strong locality is deliberately lost at the duplicate-eliminating operations the RAS paper highlights. It is acceptable for this first draft to remain unproved.[1]

## RAB extension and implementation obstacles

A substantial later variant retains E+ task selection and WithdrawDone's four-field lookup, deletion, closing and re-insertion into a freed row. Permit competing insertion/assessment/withdrawal choices at source-legal boundaries. Preserve the literal universal guard separately from replacement semantics: RAB E+ rejects candidates through its guard; RAS A.1 overwrites conflicts. These are different relations, not interchangeable encodings.[3]

`parser.mly:182–185` restricts array index sorts to proc. A faithful multi-sorted representation is not available by merely writing `Application[appIndex]`. Possible executable first draft: a shared proc carrier with immutable role tags for RowOffer/RowApplication/User/Employee/Category/Competence and validity predicates, allowing business fields to hold tagged proc values. Establish a typed-disjoint-union correspondence and check whether transition arguments' distinctness convention permits all required aliases. Do not recover Applicant=row to avoid this problem. Static DB functions can be immutable arrays on competence-tagged elements; admissible DB constraints and initialization support require inspection before claiming this is executable.

Null needs typed absence flags or properly scoped constants; invalid field values cannot acquire a business meaning accidentally. An existential competence property must be encoded through a retained witness or an equivalent absence-of-validity bad cube; that transformation needs justification. Multi-column bulk updates, nested array lookups, nondeterministic data selection, guard quantification, and per-operation processing marks all need language/solver checks. Exact Score or a justified quotient must be chosen consciously.

Current transaction control supports recursive calls and bound proc arguments and checks safety only at neutral nodes (`transaction.ml:47–75,89–108,132–161`). Those mechanisms fit this design, but do not supply relational abstraction, termination or source-refinement proofs. Internal processing marks must be reset or versioned so repeated replacement does not skip a reused row. A finite-population completion argument differs from fairness or eventual notification; stalled/infinite internal paths must not be mislabeled unsafe.

### Open proof questions / acceptance obligations

- Which exact conflict key and source discrepancy resolution is approved?
- Does the tagged encoding preserve all admissible DB structures and identity aliasing?
- Do completed row walks equal their selected source case-defined bulk updates for every finite instance and every processing order?
- Does original task interleaving refine the declared committed-state relation, or is only the stronger transactional contract claimed?
- Are reused cells fully reset, and do progress marks track the operation rather than historical row membership?
- Can the checker establish uniqueness plus competence plus score correctness without supplied invariants? If not, retain the ambitious model and identify the missing invariant/feature; do not remove conflict handling to get SAFE.
- Future concrete completion checks should exercise replacement, unaffected records, several winner rows, zero winners, and RAB withdrawal/reuse. Negative controls must violate committed properties with legal initialization and real paths. No such runs are claimed by this assessment.

## Sources

[1] https://arxiv.org/pdf/1806.11459
[2] https://arxiv.org/pdf/2208.06377v2
[3] https://raw.githubusercontent.com/AlessandroGianola/RAB-verification/main/RAB-benchmark/E+P17.txt
