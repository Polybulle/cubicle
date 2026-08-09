# Scientific foundations: array-based safety, backward reachability, and BRAB

Checked: 2026-08-09.
Sources: Cubicle publications page [1]; CAV 2012 tool paper [2] (sections 1–5); FMCAD 2013 BRAB paper [3] (sections II–IV, VI–VII); Mebsout thesis [4] (chapters 2–6). Current implementation correspondence: `kb/architecture.md`; `main.ml:60-90`; `brab.ml:26-65`; `bwd.ml:45-111`; `pre.ml:313-400`; `approx.ml:231-372`; `enumerative.ml:998-1023,1204-1221`.

Scope: this note covers baseline Cubicle/BRAB literature. For transactional MCMT, use `kb/transactional-mcmt.md`; unfinished transactional implementation work is in `kb/todo.md`.

## Scope of the checked literature

The project site presents Cubicle as an SMT-based model checker for safety of array-based parameterized transition systems: states use globals and arrays indexed by an arbitrary number of processes, with cache-coherence protocols and mutual-exclusion algorithms as principal examples.[1][2] The historical public release and publication list describe the pre-transaction tool line; they are not evidence for the 2025–26 transaction extension.

The input discipline matches the symbolic representation used by the original algorithm: initial conditions are universal constraints, while an unsafe condition is represented as an existential cube over pairwise-distinct process identifiers.[1][2] A cube therefore denotes a set of concrete states; its negation is a universal safety-style assertion. This distinction is essential when reading implementation names such as `subset`, `subsumption`, and `invariant`.

## Classical symbolic backward reachability

For a transition system \((I,T)\) and bad-state formula \(\Theta\), backward reachability begins at \(\Theta\) and iterates predecessor computation. The CAV paper describes a worklist version: remove a cube \(\varphi\), test whether \(I \wedge \varphi\) is satisfiable, discard it if it is covered by visited cubes, otherwise add it to visited and enqueue its transition predecessors.[2] (Section 3.) Exhaustion of the worklist establishes safety relative to the exact symbolic operations and coverage test; satisfiable intersection with the initial condition establishes a reachable bad path.

The expensive operation is semantic coverage: for \(\varphi = \exists\bar{x}.F\) and visited cube \(\psi = \exists\bar{y}.G\), it reduces to checking implication against appropriate substitutions from \(\bar{y}\) to \(\bar{x}\). The CAV paper explains the resulting optimization stack: normalized cubes, literal subset/contradiction filters, incremental SMT contexts, guided queue priorities, and a trie-backed visited set.[2] (Section 3.) The current source has corresponding modules (`Cube`, `Cubetrie`, `Fixpoint`, `Safety`, `Pre`), but a proof that every contemporary optimization preserves the paper's abstract algorithm remains separate work.

## BRAB: backward reachability with approximations and backtracking

The FMCAD paper introduces BRAB. It first obtains a bounded finite forward reachable set \(M\), then runs parameterized backward reachability. For a backward node \(\varphi\), it may replace \(\varphi\) with a strictly weaker candidate (a syntactic strict subcube in the Cubicle instantiation) only if that candidate is not ruled out by \(M\) or prior failed candidates.[3] (Sections II–IV.) Candidate approximations can accelerate convergence because their backward proofs are shared with the proof of the original property and other candidates.[3] (Section II.)

The finite exploration is an **oracle**, not a cutoff proof: a candidate violated by a recorded finite state is certainly unsuitable, but survival does not make it inductive or establish parameterized safety.[3] (Section III-B, Remark; Section IV.) A candidate that later yields an initial-intersecting trace is rejected as too coarse; BRAB records its ancestor and restarts. An initial-intersecting node descended solely from the original bad cube instead establishes unsafety.[3] (Section III-B.)

The generic BRAB metatheory assumes effective predecessor/posterior computation, decidable baseline backward reachability, and a finite candidate set. Under those assumptions, the paper proves: a `safe` result implies the bad condition is unreachable; an `unsafe` result implies it is reachable; and the procedure terminates.[3] (Section III-C.) The same paper explicitly limits the direct transfer of the termination/completeness result to the array-based implementation: in that setting its safety theorem applies, while termination depends on the appropriate fragment/ordering conditions.[3] (Section IV.) Do not claim unrestricted termination or completeness from a successful Cubicle run.

## Current-code correspondence and limits

The contemporary BRAB skeleton corresponds closely to the paper's control structure: `Enumerative.init` constructs finite explicit state sets; `Approx.approximations` proposes strict literal subcubes; `Enumerative.first_good_candidate` filters them against the finite states; `Bwd.search` explores them together with original bad cubes; and `Brab.search_and_backtrack` removes an approximation after an approximation-origin counterexample. See `kb/architecture.md` for the inspected call path.

This is an implementation correspondence, not a claim that current code realizes every premise of the FMCAD proofs unchanged. In particular, the current candidate enumeration includes pragmatic heuristics, the finite interpreter has explicit abstraction choices for unbounded values, and the 2026 transaction-aware paths alter both forward and backward scheduling. Any theorem-level claim for these paths needs its own transition-system semantics and preservation argument.

## Research consequences

- A finite-instance experiment can refute a candidate but cannot, by itself, prove a parameterized invariant.
- BRAB's soundness argument depends on classifying an initial hit by provenance: original versus approximation-derived. Trace/origin bookkeeping is thus correctness-sensitive, not merely diagnostic.
- Candidate finiteness matters to the generic termination result. Enlarging synthesis from strict syntactic subcubes to an unbounded semantic space needs a new termination argument or an explicitly incomplete strategy.
- A useful transaction research question is whether the transaction path relation is a semantics-preserving replacement, refinement, restriction, or abstraction of the original transition relation. The answer determines which BRAB theorem, if any, transfers.

## Open questions

- Establish a precise paper-to-current-code map for `Fixpoint.FixpointTrie.check`, `Cubetrie.delete_subsumed`, and the SMT result contract; the CAV account is architectural rather than a current-code proof.
- Read the thesis chapters on universal guards, non-atomicity, certification, and the Why3 development alongside the present tree before making claims about those features.
- Separate the 2012/2013 experimental numbers from results obtained on the present 2026 revision and toolchain.

## Sources

[1] https://usr.lmf.cnrs.fr/cubicle — Cubicle website publications
[2] https://usr.lmf.cnrs.fr/cubicle/papers/cav2012.pdf — Cubicle: A Parallel SMT-based Model Checker for Parameterized Systems (CAV 2012)
[3] https://usr.lmf.cnrs.fr/cubicle/papers/fmcad2013.pdf — Invariants for Finite Instances and Beyond (FMCAD 2013)
[4] https://usr.lmf.cnrs.fr/cubicle/papers/thesis-mebsout.pdf — Inférence d'invariants pour le model checking de systèmes paramétrés (Ph.D. thesis, 2014)
