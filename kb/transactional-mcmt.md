# Transactional MCMT / TxCubicle: developer onboarding reference

**Purpose.** This is the primary onboarding reference for work on transactional Cubicle: its intended language and semantics, the paper algorithm, the current implementation map, and the proof/engineering boundaries that must not be blurred. It is based primarily on the submitted TxCubicle paper, which is the detailed account of the feature.[1][2] Current-source observations were last checked on 2026-08-20 in this checkout; they are not proof that current code implements every paper definition unchanged. Read `kb/mcmt-notes.md` for the antecedent ATS/TATS formulation and `kb/transactions.md` for the code-level feature map and smoke-test record.

**Terminology.** This KB uses **transactional MCMT** as the umbrella term. **TxCubicle** is the paper's name for the extension. A paper-level **transaction path** is an acyclic initial-to-final trigger path over a shared process slice. The current implementation additionally elaborates `part` bodies into permutations; that extension is documented as code behavior, not silently identified with the paper relation.

## 1. The problem the feature solves

Baseline Cubicle models a parameterized protocol as a flat set of guarded transitions over globals and arrays indexed by processes. Its symbolic backward algorithm must rediscover which low-level transitions belong to the same high-level request/response behavior, and an explicit state-machine encoding of that control flow adds both control variables and remembered process identifiers to the SMT state.[1] (Introduction and §5.3.)

TxCubicle makes that control-flow information explicit. A transaction is a finite, branched path of ordinary transitions over a fixed logical slice of processes. It is **not** an ACID-style isolated critical section: ordinary low-level interleaving is still the underlying ATS setting. Rather, transactions designate sequential fragments that the model checker may use as units of control flow, path-level pre-image, and finite forward exploration.[1] (§§1.2, 2.2, 4.)

The design goal is progressive modeling. A protocol can first express optimistic request-to-stable-point paths, then be refined by adding branches, splitting paths, introducing intermediate states, or choosing a more fine-grained safety analysis. The paper's claimed benefit is structural: control flow and shared process identity are maintained outside the logical array state, reducing solver burden rather than merely adding a different syntax.[1] (§§1.2, 5.1–5.3.)

## 2. Language-level control flow

### 2.1 Surface constructs

The paper defines the following annotations.[1] (§2.2.)

- `triggered transition t(...)` means `t` is continuation-only: it cannot start a transaction.
- `triggers t1(args) or ... or tn(args)` declares the nondeterministic continuation calls after the current action.
- `yields` is a successful transaction boundary.
- Omitting a `triggers` clause implicitly yields.
- `_` in an outgoing trigger call selects a process absent from the caller transition's active process arguments.

In the present repository, `part name { ... }` is an additional current-language construct documented in `kb/transactions.md`; the paper’s principal formal account is trigger-path based. Do not silently equate the paper’s paths and the implementation’s part-permutation paths without specifying the intended relation.

### 2.2 Operational reading: success, branching, rollback

A transaction starts at an input transition, follows one selected trigger edge at each branch, and succeeds at a final/yielding transition. If any triggered step’s guard is false, that candidate execution has no committed transaction result: the complete path is rolled back and a different branch or process choice can be tried.[1] (§2.2.)

For symbolic reasoning, “rollback” should not be implemented as an extra mutable rollback state unless the model explicitly requires one. The paper’s path relation contains a boundary-to-boundary pair only when there exist intermediate states satisfying **every** constituent guard and update. In backward search, this is achieved naturally: reverse pre-image through a path produces only predecessors from which all forward guards could have succeeded.[1] (§4.3–4.4.)

### 2.3 Finiteness and transaction boundaries

Build a directed graph with one vertex per transition and one edge per declared trigger call. A vertex is **initial** unless declared `triggered`; it is **final** if it has no trigger clause or offers `yields`. Transactions are all initial-to-final paths of this graph.[1] (§4.1.)

The trigger graph must be acyclic so that path enumeration terminates and yields a finite set. Iterative protocol behavior is modeled by repeated ordinary executions across transaction boundaries, not by a cycle inside one path.[1] (§2.2, §4.1.) This is a semantic and algorithmic restriction, not merely a parser preference.

## 3. Process identity along a path

A central TxCubicle contribution is that a path shares its process choices across steps.

Consider a syntactic path

\[
 t_1(\vec{x}_1) \to t_2(\vec{x}_2) \to \cdots \to t_n(\vec{x}_n),
\]

where the call after step \(i\) supplies actuals \(\vec{y}_i\) to the formals \(\vec{x}_{i+1}\). Ignoring `_` temporarily, the paper normalizes calls by composing substitutions

\[
 \sigma_0 = \mathrm{id},\qquad
 \sigma_{i+1}=\sigma_i\circ[\vec{x}_{i+1}\mapsto\vec{y}_i],
 \qquad \vec z_i=\vec{x}_i\sigma_i.
\]

The resulting path is a sequence of transition instances over one accumulated process slice.[1] (§4.2.)

The submitted paper describes an underscore as an out-of-scope argument, distinct from the active caller arguments.[1] The current language decision differs: after resolving named actuals, underscore values need only make the complete callee actual tuple pairwise distinct. They may equal caller arguments omitted from that tuple. The selected identity is propagated by later named calls in the ordinary way, and separate calls may alias. Thus a normalized path has the form

\[
 T(\vec z)=t_1(\vec z_1)\to\cdots\to t_n(\vec z_n),
\]

where \(\vec z\) is the complete set of path variables and underscore placeholders relevant to that path execution. Callee-tuple distinctness constraints are part of the current transaction relation. This is an intentional divergence from the submitted paper and must be reflected in any revised formal account.

**Engineering implication.** The path’s shared substitution is a semantic object. Normalizing or renaming an intermediate cube without updating the stored path substitution can invalidate the connection between a predecessor and its remaining future.

## 4. Formal path semantics

For an ordinary transition \(t\), let \(\mathcal G_t(\vec a,\vec x)\) be its guard and \(\mathcal R_t(\vec a,\vec a',\vec x)\) its update relation. For a normalized path \(T\) with steps \(t_1(\vec z_1),\ldots,t_n(\vec z_n)\), the paper defines the transaction update relation as

\[
\begin{aligned}
 \mathcal R_T(\vec a_\mathrm{old},\vec a_\mathrm{new}) \;:=\;
 &\exists\vec z.\exists\vec a_1,\ldots,\vec a_{n+1}.\\
 &\bigl(\bigwedge_{1\le i\le n}
   \mathcal G_{t_i}(\vec a_i,\vec z_i)
   \wedge \mathcal R_{t_i}(\vec a_i,\vec a_{i+1},\vec z_i)\bigr)\\
 &\wedge\vec a_\mathrm{old}=\vec a_1
   \wedge\vec a_{n+1}=\vec a_\mathrm{new}.
\end{aligned}
\]

[1] (Equation 1, §4.3.)

The defining difference from independently composing ordinary transitions is the scope of \(\exists\vec z\): it is outside the conjunction of steps. The same chosen processes therefore persist throughout the path. This is the mathematical representation of the feature; it is the first equation to revisit if changing trigger arguments, fresh-variable generation, path expansion, or the interpretation of `part`.

## 5. Which safety property is being checked?

At the transaction level, the paper defines unsafety as reachability of the bad predicate at a sequence of **transaction boundaries**:

\[
 \exists T_1,\ldots,T_n.\exists\vec a_0,\ldots,\vec a_n.\;
 \mathcal I(\vec a_0)
 \wedge \bigwedge_{0\le i<n}\mathcal R_{T_i}(\vec a_i,\vec a_{i+1})
 \wedge \mathcal U(\vec a_n).
\]

[1] (Equation 2, §4.5.)

This boundary-only criterion is weaker than checking every intermediate low-level state. `-tx bwd` checks this transactional TATS property: it replaces ordinary ATS steps by complete transaction paths and reports only boundary-level safety. `-tx fwd` changes finite forward exploration for invariant generation but leaves the symbolic backward relation unchanged. `-tx all` combines both choices.

## 6. Algorithms in the paper

### 6.1 Baseline MCMT/Cubicle recap

A bad cube is an existential conjunction of literals over pairwise-distinct process variables. Baseline backward reachability maintains a work queue \(Q\) and visited cubes \(V\): it checks \(\phi\wedge\mathcal I\) for unsafety, drops \(\phi\) if it is covered by \(V\), otherwise computes pre-images for relevant transition instances and adds \(\phi\) to \(V\).[1] (§3.1–3.3.)

For a transition with \(n\) formal process arguments and a cube with \(m\) process variables, relevant instances use tuples from the \(m\) existing variables plus up to \(n\) fresh ones. This instance construction is one source of symbolic branching.[1] (§3.3.)

BRAB-style invariant generation uses a finite forward exploration as a negative oracle: a literal-deleted candidate cube is rejected when it intersects a forward-reached finite state. This is not a standalone proof that an accepted candidate is an invariant; its role is candidate filtering inside the subsequent symbolic proof.[1] (§3.4.)

### 6.2 Transaction-aware backward search

The paper replaces the iteration over ordinary transitions with an iteration over precomputed normalized paths. For each path \(T(\vec z)\), choose a relevant instance substitution once for its complete argument slice, initialize the current set to \(\{\phi\}\), then pre-image it by path steps in reverse order. The result is the pre-image of the complete path.[1] (Figure 4(b), §4.4.)

This changes both the search unit and the instantiation discipline:

- a backward edge represents an entire boundary-to-boundary path, not one low-level step;
- all step arguments are linked by the shared path substitution;
- only states allowing all forward guards of the path survive reverse pre-image;
- a path may reduce solver calls and relevant-instance branching, but it also deliberately changes the observed transition relation to the declared path relation.

The submitted paper states that, under its axiomatic path and boundary-safety semantics, this algorithm is correct and semi-complete: it terminates with `unsafe` for unsafe systems; on safe systems it returns `safe` or does not terminate.[1] (§4.5.) This theorem statement is about the paper semantics and assumptions, not a blanket guarantee for all current Cubicle configurations.

### 6.3 Transaction-aware forward exploration and BRAB

The paper proposes forward exploration along transaction paths because path-boundary states can reveal useful positive invariants without admitting arbitrary partial protocol fragments as primary finite states.[1] (§4.4.) This is especially relevant to BRAB: the finite forward result is used to reject overly general backward-cube approximations.

The paper’s German example explains the intuition: transaction-boundary `Cache` states already exhibit the intended partition—one exclusive cache and the rest invalid, or some shared caches and no exclusive cache—so the finite oracle can reject poor invariant candidates efficiently.[1] (§4.4.)

## 7. Current implementation map (observed source, 2026-08-09)

This section is an implementation map, not a new theorem.

### 7.1 Typing and elaboration

- `Typing.check_triggers` validates names and calls. `Transaction.trigger_paths` identifies inputs (`not triggered` and no parts) and outputs (yielding and no parts), rejects trigger cycles, and enumerates source paths (`typing.ml`; `transaction.ml`).
- `Transaction.resolve_call` and `path_to_futures` implement callee-tuple underscore resolution: `_` may reuse an in-scope representative absent from the current callee tuple or introduce the next canonical fresh representative; the complete callee tuple remains pairwise distinct and named arguments propagate identity (`transaction.ml`).
- `Transaction.transaction_paths` lowers every part-bearing transition into primitive part transitions and enumerates every permutation of its parts; the guarded source-level head is copied for each permutation (`transaction.ml`).
- `Transaction.expand_trigger_path` replaces part-bearing elements in a trigger path with matching permutations and applies the call substitution (`transaction.ml`).

`part` denotes the union of all serial permutations of its parts: the outer guard is checked before the first part, all parts in the selected order execute before the enclosing transaction continues, and unrelated transactions do not interleave between parts. The feature is a candidate for removal.

### 7.2 Forward path composition

With transaction-aware forward exploration, `Enumerative.compile_transaction_instance` compiles each path call under a common substitution and `compose_st_f` executes the compiled steps sequentially (`enumerative.ml:810-838`). A failed step raises `Not_applicable`, and the composed path returns no completion; this operationally realizes failed-path rollback for the finite exploration.

Intermediate successful step states are recorded in `chain_intermediates`, copied into `env.states` for BRAB candidate rejection, and deliberately omitted from the BFS queue; only complete composed results are enqueued (`enumerative.ml:806-908`). Semantically, a transaction state is `(cube, future)`, with empty `future` at a boundary. Candidate filtering compares cubes while ignoring `future`; it therefore uses intermediate states as negative evidence even though BFS growth is boundary-oriented. This may reject a candidate that holds only at boundaries, but it cannot establish a candidate and does not alter the core symbolic backward proof.

### 7.3 Backward path pre-image

When `Options.tx_bwd` is set, `Pre.pre_image` selects `pre_image_path` rather than ordinary `pre_image_normal` (`pre.ml:343-400`). A node without a future is expanded over path substitutions; a node with a future is pre-imaged by its next path transition, then its remaining future is processed recursively before any final nodes are returned to the queue.

While a node still has a future, `Pre.cube` avoids normal renaming (`Cube.elim_ite_simplify_unnorm`) because its `Node.toward` global substitution refers to original variable names (`pre.ml:245-252`). Correspondingly, `Bwd.search` skips direct initial-state safety and fixpoint checks for nodes with a future (`bwd.ml:67-69`) and only adds future-free nodes to visited (`bwd.ml:91-97`). These are correctness-sensitive representation invariants, not superficial optimizations.

### 7.4 BRAB provenance remains decisive

`Brab.search_and_backtrack` distinguishes an unsafe node with original provenance from one descended from an approximation. It returns the former, but removes/restarts after the latter (`brab.ml:26-45`). Transactions do not eliminate this obligation: finite transaction-aware forward states still filter candidates only; a reported parameterized proof depends on the symbolic backward search and provenance handling.

## 8. Native paths versus a compilation encoding

The paper gives a reference encoding of the path automaton back into ordinary Cubicle: add an enumerated control variable, persistent process-valued path argument variables, a dispatch transition for every path, and guarded copied transitions for every path position.[1] (§5.3.)

This is useful as a semantic comparison and potential differential-test oracle. It has an expected cost: SMT must reason about extra finite control and, more significantly, process-index-valued memory variables tying a control location to its selected path arguments. Native TxCubicle instead stores/processes that path structure externally.[1] (§5.3.)

Any future native-algorithm change should ideally be tested against this compilation on small, hand-checkable models for the selected safety criterion. Equality of a few verdicts is not itself a semantics proof; the encoding relation, treatment of intermediate bad states, and rollback behavior must be stated first.

## 9. Experimental claims and their limits

These are results reported by the submitted paper, not measurements from the present checkout.

| Benchmark/configuration | Paper-reported result |
|---|---|
| German, no transaction structure, baseline | 38 ms; 19 cubes; 26 skipped; 84 SMT calls; 3 processes |
| German, compiled transactions with baseline | 74 ms; 54 cubes; 86 skipped; 354 SMT calls; 4 processes |
| German, native path algorithm | 31 ms; 3 cubes; 8 skipped; 3 SMT calls; 2 processes |
| FLASH, baseline backward + baseline forward | 2.5 s; 191,649 forward nodes; 37 backward nodes |
| FLASH, baseline backward + transaction-aware forward | 0.6 s; 56,099 forward nodes; 37 backward nodes |
| FLASH, transaction-aware backward + forward | 0.7 s; 56,099 forward nodes; 31 backward nodes |

[1] (Table 1, §5.4.)

The German comparison supports the structural-versus-compilation claim under the paper’s benchmark configuration. The FLASH comparison supports the claim that transaction-guided finite exploration can substantially improve BRAB guidance in that experiment. Neither table proves semantic equivalence, universal speedup, solver-independent performance, or the behavior of the current repository revision. Rerun with recorded commands, inputs, revision, solver/toolchain, and resource limits before using these figures in a current evaluation.

## 10. Non-negotiable engineering checklist

Before editing any transaction-aware code, identify all of the following.

1. **State relation:** Is the change to the ordinary low-level ATS relation, the transaction-boundary relation, or just a search heuristic over an unchanged relation?
2. **Observation point:** Is `unsafe`, initial intersection, fixpoint/subsumption, candidate filtering, and trace reporting intended at a boundary, at every step, or separately for each subsystem?
3. **Parameter discipline:** Which variables are shared across a path? How do named actuals and `_` introduce/reuse/freshen process identities?
4. **Path finiteness:** Does the change preserve finite, acyclic path extraction? If not, what replaces exhaustive enumeration?
5. **Representation invariants:** Does a cube’s variable naming still agree with `Node.toward` substitutions and remaining path calls?
6. **Rollback:** Does a failing constituent guard discard the full attempted path rather than accidentally admit a prefix as a completed path?
7. **BRAB soundness:** Is a finite forward result used only as a candidate-disqualifying oracle, and are original versus approximation-origin unsafe traces still distinguished?
8. **Validation:** Construct a minimal model distinguishing boundary-only from intermediate-state safety; run known-safe and known-unsafe cases; compare native and compiled semantics where feasible; validate traces independently.

## 11. Known limits and research directions

- Trigger paths are acyclic; loops currently require crossing transaction boundaries.[1] (§2.2, §4.1.)
- Intermediate path nodes are intentionally under-used by the current fixpoint checker; the paper proposes exploiting them for earlier coverage decisions.[1] (§6.)
- The paper identifies richer normalization/compilation of iterative constructs and controlled interleaving among concurrent transactions as future work.[1] (§6.)
- Transaction annotations are a modeler-level semantic commitment. Their correctness is not inferred from a flat ATS automatically.

## Sources

[1] file:///Users/hector/Desktop/ICFEM26/paper.tex — Transactional Cubicle submitted paper (LaTeX source)
[2] file:///Users/hector/Desktop/ICFEM26/paper.pdf — Transactional Cubicle submitted paper (PDF)
