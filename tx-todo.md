# Transaction backward-search TODO

This first implementation makes control context explicit instead of reconstructing it from diagnostic history. The numbered steps below are implementation work; the design notes state the rules that work must preserve.

## Design notes

- **Boundary semantics.** Initial and unsafe configurations are observed at transaction boundaries, preserving the older transaction semantics.
- **Neutral is not dummy.** `Types.neutral_name` names the boundary control point. It has no active transaction-parameter bindings and no executable transition. `Types.dummy_name` tags nodes without a concrete control position; it must not confer global applicability on supplied invariants.
- **Supplied invariants are boundary-only and trusted.** In backward transaction mode, `invariant (x ...) { phi }` asserts that the negative cube is unreachable at neutral states only. Its node may cover only neutral obligations, and its negated instances remain assumptions in initial-state checks. Internal states may violate it. Cubicle does not prove this assertion. This supersedes the earlier all-control-state interpretation. Preserve ordinary invariant behavior without backward transaction mode; state-local invariant syntax is deferred to step 11.
- **The forward oracle remains a rejection filter.** Forward exploration has its own state representation. It may discard useful candidates, but must not certify their unreachability or replace backward verification. Its `Node.t` candidate renamings are covered by step 4. Forward and enumerative exploration changes are deferred to step 10.
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

### REOPENED 9. Review covering at arbitrary states

For states `Before t1(xs1)` and `Before t2(xs2)`, subsumption requires `t1 = t2` and must be checked under a most general unifier (MGU) of `xs1` and `xs2`. This is the meaning of compatible bindings. Apply the unifier consistently to the associated cubes.

Apply that rule to every covering path, including quick trie checks, SMT fixpoints, and subsumption-based deletion; merely gating the node being checked is insufficient. Supplied `Inv` nodes cover only neutral states, never internal obligations. Any resolution or normalization of located nodes must preserve a justified control context. Approximation selection remains boundary-only until step 11.

The experimental internal covering is now commented out pending review. The fused
`Fixpoint.Located` module and `Covers` alias have been removed. Search uses the
existing `Cubetrie` storage and `FixpointTrie` checker, with boundary policy in
`bwd.ml`. Internal obligations are expanded without covering, storage, or deletion;
their accounting and limits remain active. A checker-only version of the former
normalization and restricted-instantiation proposal is retained in a comment.

Controlled before/after tests found that the two safe internal-loop cases lose
their SAFE verdicts and reach node limits in both schedulers. A reachable-cycle
case whose bad predecessors cannot enter the loop still proves SAFE. The unsafe
loop-exit case remains UNSAFE under BFS and the tested parallel schedules, but
sequential DFS with postponement 0/1 reaches the node limit. See
`tests/located-covering/README.md` for the 84 bounded comparisons and regressions.
Review the actual internal coverage algorithm before restoring it. Transaction
certificate generation remains unverified.

### 10. Adapt forward and enumerative exploration

Adapt forward and enumerative exploration to cyclic transactions, including neutral traversal. This later step is under Tetra's purview as an experiment in agentic coding. Agree its detailed specification separately; it is not part of step 8.

### 11. Add local invariants

Explore state-local invariants in a later, separately specified step, including internal approximation candidates and the corresponding candidate identity, rejection, bad-candidate storage, and restart handling. Until this step, both supplied invariants and approximation candidates apply only at neutral states in backward transaction mode.
