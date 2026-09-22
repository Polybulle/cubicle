# Transaction backward-search TODO

This first implementation makes control context explicit instead of reconstructing it from diagnostic history. The numbered steps below are implementation work; the design notes state the rules that work must preserve.

## Design notes

- **Boundary semantics.** Initial and unsafe configurations are observed at transaction boundaries, preserving the older transaction semantics.
- **Neutral is not dummy.** `Types.neutral_name` names the boundary control point. It has no active transaction-parameter bindings and no executable transition. `Types.dummy_name` tags nodes without a concrete control position; it must not confer global applicability on supplied invariants.
- **Supplied invariants are boundary-only and trusted.** In backward transaction mode, `invariant (x ...) { phi }` asserts that the negative cube is unreachable at neutral states only. Its node may cover only neutral obligations, and its negated instances remain assumptions in initial-state checks. Internal states may violate it. Cubicle does not prove this assertion. This supersedes the earlier all-control-state interpretation. Preserve ordinary invariant behavior without backward transaction mode; state-local invariant syntax is deferred to step 11.
- **The forward oracle remains a rejection filter.** Forward exploration has its own state representation. It may discard useful candidates, but must not certify their unreachability or replace backward verification. Its `Node.t` candidate renamings are covered by step 4. Located forward traversal is implemented in step 10; intermediate data observations still participate in heuristic rejection.
- **Candidates remain at neutral until step 11.** In backward transaction mode, do not generate approximation candidates at internal states. Reject any internal-state candidate supplied to candidate handling rather than attempting to prove it or retaining it across a restart. General located-candidate machinery is deferred to exploratory step 11.
- **Scope through step 8.** Boundary-only covering and approximation selection are retained. Internal fixpoints and subsumption belong to step 9; boundary-only covering does not absorb internal cycles.
- **Commit discipline.** Every step gets its own minimal commit. Do not combine separate steps or include unrelated changes.

## Implementation steps

### DONE 1. Add an explicit state field to `node_cube`

Each search node must carry its control location and the process bindings needed to continue backward exploration. Keep this context separate from `from`, which remains the diagnostic history. The CFG belongs to `t_system`, but the current control context belongs to each node. Keep trusted `Inv` nodes distinct from obligations that require verification; their applicability is governed by the boundary-only design note above.

### DONE 2. Add the neutral control point to the CFG

Connect yielding exits to neutral and neutral to non-triggered entries. Include neutral in successor and predecessor tables, with an empty argument list, but not in the executable transition table. Backward traversal of these edges belongs to step 5.

### 3. DONE Give `Orig` nodes the neutral state

Pass `Node.neutral_pos` when typing unsafe roots. The constructor audit found no other production unsafe-root creation site. Supplied invariants previously used `Node.dummy_pos`; aligning their representation with the boundary-only semantics is part of step 8.

### DONE 4. Preserve state when creating and renaming approximations

An `Approx` node inherits the full control context of the node it approximates, even if its diagnostic history is reset. Preserve bindings that remain necessary for control flow when approximation drops their variables from the cube. Apply the formula renaming consistently to control bindings, without collisions with control-only variables, in both approximation normalization and enumerative candidate alpha-renaming.

### DONE 5. Replace history-based predecessor dispatch and split scheduling

Predecessor calls and their argument bindings now come from the explicit state, not the head of `from`. Each pre-image produces the corresponding predecessor state as well as its cube. History remains diagnostic and does not select a control location.

Neutral predecessor calls short-circuit before executable-transition lookup. Each returns a fresh neutral node with the same cube, witnesses, history, and depth, but no active control bindings. Other predecessor calls still compute their executable pre-images.

Sequential and parallel scheduling now separate boundary processing from internal expansion. Neutral nodes undergo safety and fixpoint checks, then compute pre-images through yielding transitions. Internal nodes skip those checks and approximation selection, but retain resource checks, accounting, and predecessor expansion. The boundary-check refactor groups these operations rather than guarding each separately. This split is enabled only by backward transaction mode; ordinary scheduling is preserved without `-tx`.

### DONE 6. Make initial-state intersection state-aware

The scheduler checks initial-state intersection only at neutral states in backward transaction mode, including approximation checks and parallel-search paths. Matching initial data at an internal location no longer makes that configuration initial. Supplied invariant assumptions and the zero-step check remain intact, including for models with no entry transitions.

### DONE 7. Restrict fixpoints, subsumption, and deletion to boundaries

Boundary-only covering is implemented. In backward transaction mode, only neutral search nodes enter the existing cube-only covering trie, and fixpoint checks and subsumption deletion run only at boundaries. Parallel temporary visited sets follow the same rule. Supplied invariants remain in the trie as boundary covers. Internal nodes are expanded without being stored as covers; general internal-state covering remains open.

### DONE 8. Enforce neutral-only candidates and validate backtracking

Restrict approximation candidates to neutral states throughout generation, candidate handling, and restarts. Do not implement general candidate identity or bad-candidate storage across different control states in this step. Preserve the distinction between a rejected approximation and a counterexample to an original unsafe root. Rejecting an internal-state candidate does not mean dropping internal predecessor obligations generated while checking a neutral candidate.

Align supplied-invariant representation and use with neutral-only applicability in backward transaction mode. Keep them trusted and retain their assumptions in initial-state checks. Audit consumers for assumptions of global applicability; do not change ordinary invariant behavior outside backward transaction mode.

Keep covering and approximation selection boundary-only in this step. Leave forward rejection heuristics to step 10.

Implemented and checked by `tests/neutral-candidates/`: sequential tests on OCaml
5.4.1 and parallel tests with real Functory in an isolated OCaml 4.12.0 build.
Both schedulers reached Cubicle's node limit on an internal loop. `make test`
passed. See the suite README for commands, expected outcomes, and evidence limits.

#### Agreed acceptance tests

Use focused internal tests and end-to-end models. Choose concrete fixtures after inspecting the existing tests.

- A supplied invariant may be violated internally and restored before yielding; it must not prune the internal obligation.
- Generate no internal candidates. Discard explicitly supplied internal candidates, including on restart, without dropping internal predecessor obligations of neutral candidates.
- Reject a reachable approximation without reporting the original property unsafe. Preserve usable surviving candidates across restarts.
- Report a genuine counterexample to an original unsafe root as `UNSAFE`, including when its formula coincides with a rejected candidate.
- Preserve supplied-invariant assumptions in initial checks, zero-step checks, and behavior for models without entry transitions.
- Exercise sequential and parallel backward paths, safe and unsafe cases, and ordinary non-transaction behavior. Run `make test` under the resource safeguards below.
- Expect repeated internal-cycle exploration to prevent `SAFE` verdicts before step 9. Do not require termination of such cases or count a resource limit as a verdict. Report any blocking forward-exploration dependency rather than expanding step 8.

#### Mandatory resource safeguards

Always bound verifier test runs with timeouts and/or node limits. Enforce an external wall-clock timeout as well, at least until tests establish that internal cubes count toward Cubicle's enforced resource limits. Apply this safeguard to focused runs and regression suites, including child verifier processes. Verify internal-node accounting and limit enforcement explicitly in sequential and parallel search; source inspection alone is not sufficient evidence. Record the limits used and distinguish Cubicle limit exits, external timeouts, crashes, and conclusive verdicts.

### REOPENED 9. [Implement fixpoints over located cubes](tx-fixpoint.md)

The design, implementation requirements, and recorded evidence now live in
`tx-fixpoint.md`.

### DONE 10. Adapt forward and enumerative exploration

Both engines now traverse `(data, control location, bindings)` in transaction-forward
mode. The CFG resolves calls over a fixed finite process domain. Neutral traversal
executes no transition; internal traversal follows only prescribed calls and yields.
Visited keys include control context. Named bindings and callee-tuple distinctness
are preserved, including underscore reuse of caller arguments omitted from the tuple.

Keep finite process identities fixed rather than applying formula-only renaming or
data-only symmetry normalization. Ordinary forward behavior is unchanged. Symbolic
stateless exploration uses the same located traversal. Candidate rejection still
uses intermediate data observations as a heuristic, not a located reachability proof.

Symbolic exploration precompiles one canonical instance per transition over the
finite domain. Each event instantiates it through a full-domain permutation using
`subst_inst_transition`; no compiled-and-instantiated event cache is retained.
Compilation and permutation equivalence are checked on universal guards, array
updates, actions, and touched terms.

Depth limits count executable steps; configuration budgets also count neutral and
internal configurations. These budgets are not directly comparable to old data-state
counts. The visited depth is improved when a shorter path reaches the same configuration.

`tests/forward-transactions/` compares both engines with an independent finite-state
interpreter: 315 differential cases passed on OCaml 5.4.1 and 4.12.0. Six end-to-end
BRAB checks passed sequentially and with real Functory. Step-8/9 regressions and
`make test` passed. All runs had external timeouts. See the suite README for the
German budget experiment, remaining abstraction assumptions, and reproduction commands.

### 11. Add local invariants

Explore state-local invariants in a later, separately specified step, including internal approximation candidates and the corresponding candidate identity, rejection, bad-candidate storage, and restart handling. Until this step, both supplied invariants and approximation candidates apply only at neutral states in backward transaction mode.

### 12. Localized Approximations 

TBD.
