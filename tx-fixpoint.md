# Fixpoints over located cubes

This note describes the agreed design for step 9 of
[the transaction backward-search work](tx-todo.md). The design makes control
part of the symbolic state. The fixpoint check uses both the data predicate and
the control position. This lets backward search detect subsumed nodes inside transactions,
not just at their boundaries.

The design is settled, but its implementation remains open. Solver integration,
witness handling, and proof obligations still need review. The final section
records the implementation status and test results reported in the former step 9.

## 1. A cube describes data and control together

A TxCubicle cube contains a data predicate, a control location, and active process
bindings. For example, `Before t(p, q)` identifies the location before `t` and
binds its arguments to `p` and `q`. Two cubes can describe the same data but
different control states. The covering check must preserve that difference.

We use a flat algebraic datatype as the semantic reference. The datatype has one
constructor for each control location. Each constructor carries that location's
active process arguments. The `Neutral` constructor represents a transaction
boundary and carries no arguments.

```text
Position = Neutral | At_t1(proc, ...) | ... | At_tn(proc, ...)
```

One shared state symbol `K` records the control position. A neutral cube uses `K
= Neutral` instead of a constructor with arguments. Every cube refers to the
same state symbol `K`. No cube existentially quantifies `K`. 

For a cube `C` with data predicate `exists ys. phi` and position `Before tj(z1,
..., zk)`, the encoding is:

```text
E(C) = exists W. D(W) && phi && K = At_tj(z1, ..., zk)
```

The process set `W` contains all data processes `ys` and active control
processes `zs`, without duplicates. The predicate `D(W)` expresses the distinctness
constraints. Every renaming must act consistently on the data and the control
bindings.

## 2. Covering compares sets of located states

The visited cubes cover a goal when their union contains every state of the goal:

```text
E(goal) => OR { E(cover) | cover in visited }
```

The fixpoint checker must preserve this covering judgement from baseline
Cubicle. Several covers or several instances may jointly cover one goal. Note
that TxCubicle also implements equality checks and pairwise syntactic
subsumption as preliminary, heurisitic checks, independently of the more
expensive canonical judgment above.

The control part of a cube determines which covers apply. A cover at `Before
t(p)` does not cover a goal at `Before u(q)` merely because their data
predicates match. A cover at `Before t(p)` doesn't directly apply to a cube at
`Before t(q)`, but its alpha-renamed image under `[q/p]` does. The checker must
carry that alignment into the data predicates.

## 3. Constructor equality gives the control rules


The control theory only needs quantifier-free reasoning. Constructor arguments,
which have sort `proc`, may be existentially quantified via Cubicle's existing
instantiation machinery. This step needs no recursive datatypes, induction,
general quantified datatype reasoning, or exposed selectors and testers.

The control theory needs constructor disjointness and injectivity. Different
constructors are unequal. Equal applications of the same constructor have equal
corresponding arguments.

```text
At_t(xs) = At_u(ys)    is false when t and u differ
At_t(x1, ..., xn) = At_t(y1, ..., yn)
                       reduces to x1 = y1 && ... && xn = yn
```

Constructor disequalities need the corresponding Boolean treatment. Their
argument equalities must interact with data reasoning. 

Note that uninterpreted constructor functions alone do not provide these rules.
 
## 4. A located goal allows an exact specialization

Every covering goal retains its full location and active bindings: `K` is always
equal to a constructor application. This fact removes the need to split the
query over all constructors: covers are naturally partitioned over transition
names.

The checker verifies which partition a goal belongs to at every covering entry
point. The specialized check discards covers at other constructors. This reduces
same-constructor equalities to argument equalities, which are merely
conjunctions of process equalities. It then checks the relevant data predicates
with those equalities in place. The implementation may eliminate the equalities
through substitution only if it justifies the substitution and applies it
consistently to the cover's data. Note that equalities may cause the
distinctiveness relation to be temporaliry broken. This must not be mistaken for
inconsistency of the cube. 

This specialization does not need a constructor-exhaustiveness axiom. The goal
already fixes the constructor. Covers either come from a visited nodes, that
also have locations, or from invariants and unsafe nodes, that only apply at the
neutral position. 

The implementation may use a small constructor theory or an exact reduction to
existing equality reasoning. Inspection of the solver's extension points will
determine that choice. Both implementations must realize the same reference query.

## 5. Covering does not forget control

Unions of data predicates do not implement coverage at every location: even when
`psi => OR_i phi_i` holds, the located cubes `phi_i && K = At_ti(args_i)` need
not cover `psi && K = t'(args')`. Replacing visited located sets with un-located
is an approximation that requires verification, but a routine fixpoint check.

A control-independent cover can cover a located goal without an exhaustiveness
axiom. Supplied invariants and originunsafe nodes are not such covers: they
apply only at neutral states. A node having a dummy position does not grant
global applicability. 

Approximation selection remains neutral-only. Candidate construction
must preserve the position, so a neutral approximation stays neutral.
Initial-state intersection also remains neutral-only.

Approximations in non-neutral positions need a separate sprint for design in
implementation, out of scope here. 

## 7. Every covering path must use the located meaning

The implementation must apply located covering throughout the search. The scope
includes quick trie checks, SMT fixpoint checks, cover storage, subsumption
deletion, and temporary visited sets in both schedulers. A guard on the queried
node alone does not suffice. Resolution and normalization must also preserve the
control context.

The change must preserve the surrounding search rules. Initial-state intersection,
supplied invariants, and approximation selection remain neutral-only. Ordinary
non-transaction behavior remains unchanged. Internal nodes retain accounting and
resource-limit checks.

## 8. Proof and tests have separate roles

The correctness argument must discharge these obligations:

- The encoding represents the intended located configurations.
- The process representation preserves the intended distinctness rules.
- Control-equality elimination preserves the reference query.
- Every covering goal fixes a constructor, which justifies omitting exhaustiveness.
- The proposed implementation preserves baseline witness-instantiation behavior.

Tests must compare the implementation with explicit control encodings. Focused
cases must cover different locations with identical data, swapped or different
active bindings, control-only witnesses, extra cover witnesses, and union coverage
that needs multiple instances or covers. The checks must exercise quick covering,
SMT covering, and deletion. They must also check neutral-only applicability of
supplied invariants and approximations.

Regression runs must include safe and unsafe models with internal loops in both
schedulers, ordinary non-transaction cases, and `make test`. Every verifier run
needs a timeout and/or a node limit. An external wall-clock timeout must also
bound runs until tests establish that internal cubes count toward enforced
limits. This safeguard applies to child verifier processes and regression suites
as well as focused runs. Tests must explicitly check internal-node accounting
and limit enforcement in both schedulers. Reports must record the limits and
distinguish conclusive verdicts, Cubicle limit exits, external timeouts, and
crashes.

Passing tests does not discharge the proof obligations. The design also makes no
termination claim for arbitrary transaction loops. 

Performance needs measurement. Shared control symbols do not add process witnesses
to permute, but active control-only witnesses can enlarge the instantiation
support. Experiments should compare explicit and specialized control reasoning
where feasible. They should measure the cost rather than assume that datatype
control is prohibitively expensive.

## 9. Recorded implementation status and historical evidence

The former step 9 recorded that experimental internal covering had been commented
out pending review. The fused `Fixpoint.Located` module and its `Covers` alias had
been removed. Search used `Cubetrie` storage and the `FixpointTrie` checker, with
the boundary policy in `bwd.ml`. Internal cubes had no covering, storage, or
deletion, but retained accounting and limits. A comment preserved a checker-only
version of the former normalization and restricted-instantiation proposal.

The recorded before/after tests showed a loss of internal-loop coverage. Two safe
internal-loop cases lost their `SAFE` verdicts and reached node limits in both
schedulers. A reachable-cycle case still proved `SAFE` because its bad
predecessors could not enter the loop. The unsafe loop-exit case remained `UNSAFE`
under BFS and the tested parallel schedules. Sequential DFS reached the node limit
with postponement 0 or 1.

The historical report points to `tests/located-covering/README.md` for 84 bounded
comparisons and regressions. These results are prior evidence, not new validation
of this design. The actual internal coverage algorithm still needs review before
restoration. Transaction certificate generation remains unverified.
