# MCMT / ATS notes: formalization and transaction questions

Checked: 2026-08-09. Primary source: Hector Suzanne, *Notes on MCMT and transactions*, LaTeX source and PDF compiled 2026-05-09.[1][2] This is a reading map of the notes, not an independent validation of every stated theorem. Where the notes extend MCMT with transactions, do not infer that the current Cubicle implementation has this exact semantics; compare `kb/transactions.md`.

## Baseline model-theoretic setting

The notes use many-sorted first-order signatures and structures, then distinguish quantifier-free, sort-restricted existential, and universal formulas. They take embeddings to be injective homomorphisms that preserve and reflect predicate interpretations; quantifier-free formulas are preserved and reflected, existential formulas are preserved forward, and universal formulas are reflected backward.[1] (Sections 1.1–1.2.)

The intended array setting has separate index and element theories \(\mathcal T_I\) and \(\mathcal T_E\), plus a fresh array sort \(A\) interpreted as functions from indices to elements. An array transition system (ATS) has an initial predicate, a negative-form unsafe predicate, and finitely many guarded, case-defined updates with explicit index parameters.[1] (Sections 2.1–2.2.)

Under the stated MCMT-style hypotheses—existential-index unsafe formula, universal-index initial formula, quantifier-free guards, and quantifier-free case-defined updates—the notes state that pre-images preserve existential-index form. With decidable index and element theories, plus local finiteness and substructure closure for the index theory, the resulting \(\exists^A\exists^I\forall^I\) satisfiability checks are decidable; the safety problem is then semi-decidable by pre-image enumeration.[1] (Section 2.4.) This is a conditional theorem schema, not a claim that every Cubicle input or option satisfies the premises.

## Cubes and ordinary backward reachability

A cube is a primitive differentiated existential-index formula: a conjunction of literals existentially quantified over index variables, including all pairwise disequalities. The notes give a normalization path from existential-index formulas to finite disjunctions of cubes, so pre-image may be treated as mapping a cube to a finite set of cubes.[1] (Section 3.1.)

The proposed worklist/rule presentation maintains visited and pending unsafe cubes. It has rules for predecessor expansion after an initial-state disjointness check, removal of unsatisfiable cubes, coverage/subsumption removal, `UNSAFE` on an initial intersection, and `SAFE` on empty pending work.[1] (Section 3.2.) The notes' proof sketch makes the core invariant explicit: the remaining and visited cube sets must continue to represent the relevant backward closure; this is the semantic obligation behind operational pruning.

## Transactional ATS (TATS)

A transaction is defined as a finite sequence of ordinary transition names together with substitutions from each step's formal parameters into one shared collection of transaction variables. A TATS is an ATS plus a family of such transactions; the trivial TATS contains the one-step transaction for every underlying transition.[1] (Section 2.3.)

A transaction relation \(\tau_t(a,a')\) existentially quantifies the common transaction arguments and composes the step relations through explicit intermediate arrays. The global relation is the disjunction over declared transactions. A TATS run composes whole transactions, not arbitrary individual steps.[1] (Section 2.3.) Therefore the notes' **transactional safety** asks about bad states only at transaction boundaries; **structured safety** additionally observes intermediate states; and **low-level safety** is ordinary safety of the underlying ATS.

The notes establish the direction

\[
  \text{low-level safety} \Longrightarrow \text{structured safety}
  \Longrightarrow \text{transactional safety},
\]

with no converse in general, illustrated by a two-step increment/decrement transaction whose boundary states hide an intermediate state.[1] (Example 2.17.) This is the key semantic fork for future Cubicle transaction work: a checker must say which of these properties it reports, and whether its unsafe predicate is evaluated at boundaries, intermediate states, or both.

For transactional safety, the notes state that the standard pre-image analysis applies after replacing ordinary transitions by transaction relations. The closure argument in Section 2.4 relies on each transaction being finite and each constituent update retaining the specified quantifier-free case form.[1] (Section 2.4.) A proof for structured or low-level safety needs an explicitly selected observation relation; it should not be obtained merely by reusing a boundary-only theorem.

## Current transaction contract

`-tract bwd` checks transactional safety at transaction boundaries, whereas
`-tract fwd` changes only finite invariant generation. The transaction
semantics, including caller-local underscore selection and forward treatment of
intermediate states, is integrated in `kb/transactional-mcmt.md`. Unfinished
underscore implementation work is in `kb/todo.md`.

## Sources

[1] file:///Users/hector/LMF/template/notes.tex — User MCMT notes (LaTeX source)
[2] file:///Users/hector/LMF/template/notes.pdf — User MCMT notes (PDF)
