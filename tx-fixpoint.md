# Fixpoints over located cubes

This note describes the agreed design for step 9 of
[the transaction backward-search work](tx-todo.md). The design makes control
part of the symbolic state. The covering check uses both the data predicate and
the control context. This lets backward search detect coverage inside transactions,
not just at their boundaries.

The design is settled, but its implementation remains open. Solver integration,
witness handling, and proof obligations still need review. The final section
records the implementation status and test results reported in the former step 9.

## 1. A cube describes data and control together

A located cube contains a data predicate, a control location, and active process
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

One shared state symbol `K` records the control position. For a cube `C` with data
predicate `exists ys. phi` and position `Before tj(z1, ..., zk)`, the encoding is:

```text
E(C) = exists W. D(W) && phi && K = At_tj(z1, ..., zk)
```

The witness set `W` contains all data witnesses and active control witnesses,
without duplicates. A control-only witness belongs to `W` even when `phi` does
not mention it. The predicate `D(W)` expresses the intended distinctness
constraints. The implementation must establish the combined witness invariant;
it must not silently add disequalities. Every renaming must act consistently on
the data and the control bindings.

A neutral cube uses `K = Neutral` instead of a constructor with arguments. Every
cube refers to the same state symbol `K`. No cube existentially quantifies `K`.

## 2. Covering compares sets of located states

The visited cubes cover a goal when their union contains every state of the goal.
The reference criterion is:

```text
E(goal) => OR { E(cover) | cover in visited }
```

The checker must preserve ordinary Cubicle covering on this explicit encoding.
Several covers or several instances may jointly cover one goal. Equality checks
and pairwise syntactic subsumption do not replace this criterion. An optimized
sufficient test must not silently narrow the reference covering test or its
instantiation opportunities.

Control determines where a cover applies. A cover at `Before t(p)` does not cover
a goal at `Before u(p)` merely because their data predicates match. A cover at
`Before t(p)` also needs argument alignment before it can cover a goal at
`Before t(q)`. The checker must carry that alignment into the data predicates.

## 3. Constructor equality gives the control rules

The control theory needs constructor disjointness and injectivity. Different
constructors are unequal. Equal applications of the same constructor have equal
corresponding arguments.

```text
At_t(xs) = At_u(ys)    is false when t and u differ
At_t(x1, ..., xn) = At_t(y1, ..., yn)
                       reduces to x1 = y1 && ... && xn = yn
```

Constructor disequalities need the corresponding Boolean treatment. Their
argument equalities must interact with data reasoning. Uninterpreted constructor
functions alone do not provide these rules.

The control theory only needs quantifier-free reasoning. Constructor arguments
have sort `proc`, never sort `Position`. Cubicle's existing instantiation machinery
still handles process witnesses. This step needs no recursive datatypes,
induction, general quantified datatype reasoning, or exposed selectors and testers.

## 4. A located goal allows an exact specialization

Every covering goal must retain its full location and active bindings. Its
positive formula therefore fixes `K` to one constructor application. This fact
removes the need to split the query over all constructors. The checker must verify
this precondition at every covering entry point.

The specialized check discards covers at other constructors. It reduces
same-constructor equalities to argument equalities. It then checks the relevant
data predicates with those equalities in place. The implementation may eliminate
the equalities through substitution only if it justifies the substitution and
applies it consistently to the cover's data. The check must retain union coverage
across all relevant instances.

This specialization does not need a constructor-exhaustiveness axiom. The goal
already fixes the constructor. That argument applies to these queries only; it
does not justify omitting exhaustiveness from arbitrary datatype reasoning.
Goals that forget their constructor fall outside this step.

A tag-and-slots encoding provides another presentation of the same reference.
It uses one distinct tag per location and shared process-valued argument slots.
Only active slots equal the control arguments. Inactive slots remain unconstrained;
they do not become extra distinct witnesses. Datatype constructors avoid exposing
these unused slots and express argument alignment directly.

The implementation may use a small constructor theory or an exact reduction to
existing equality reasoning. Inspection of the solver's extension points will
determine that choice. Both implementations must realize the same reference query.

## 5. Argument alignment must preserve witness instantiation

Control alignment constrains arguments, but it does not define the entire
instantiation algorithm. The earlier most-general-unifier description was not a
complete specification. A fixed injective mapping does not, by itself, justify
restrictions on the remaining witness instantiations.

The former step 9 identified a gap in the commented experimental checker. That
checker normalizes combined data and control witnesses. It enumerates injective
cover instances with fixed control bindings. It rejects a cover when the cover
needs more remaining witnesses than the goal supplies. By contrast, baseline
`Instantiation.relevant` extends the target support through `Variable.extra_vars`.

The implementation must resolve this gap before restoring internal covering.
Constructor alignment does not justify the restriction. Review must compare the
checker with the reference query and preserve baseline instantiation behavior.

## 6. Covering does not forget control

A union of data predicates does not establish coverage at every location. Even if
`psi => OR_i phi_i` holds, the located cubes
`phi_i && K = At_ti(args_i)` need not cover `psi` at the goal's location and bindings.
Replacing visited located sets with a larger predicate is an approximation that
requires verification. It is not a fixpoint simplification.

A genuinely control-independent cover could cover a located goal without an
exhaustiveness axiom. Current supplied invariants are not such covers. They remain
trusted assertions that apply only at neutral states. A dummy position does not
grant global applicability. Neither does the absence of control from a data cube.

Approximation selection remains neutral-only in this step. Candidate construction
must preserve and rename the position, so a neutral approximation stays neutral.
Initial-state intersection also remains neutral-only.

Internal approximation needs a separate candidate lifecycle. The semantics does
not forbid it, but the current lifecycle cannot support it. Data-only identity
can confuse an internal candidate with an original unsafe root. Data-only
rejection filters can also discard useful candidates at other locations.
Neither operation proves located reachability. Step 11 retains responsibility
for location-aware identity, rejection, and restart handling. Forgetting control
is a separate extension.

## 7. Every covering path must use the located meaning

The implementation must apply located covering throughout the search. The scope
includes quick trie checks, SMT fixpoint checks, cover storage, subsumption
deletion, and temporary visited sets in both schedulers. A guard on the queried
node alone does not suffice. Resolution and normalization must also preserve a
justified control context.

The change must preserve the surrounding search rules. Initial-state intersection,
supplied invariants, and approximation selection remain neutral-only. Ordinary
non-transaction behavior remains unchanged. Internal nodes retain accounting and
resource-limit checks.

## 8. Proof and tests have separate roles

The correctness argument must discharge these obligations:

- The encoding represents the intended located configurations.
- The combined witness representation preserves the intended distinctness rules.
- Control-equality elimination preserves the reference query.
- Every covering goal fixes a constructor, which justifies omitting exhaustiveness.
- Specialization preserves baseline witness-instantiation behavior.

Tests must compare the implementation with explicit control encodings. Focused
cases must cover different locations with identical data, swapped or different
active bindings, control-only witnesses, extra cover witnesses, and union coverage
that needs multiple instances or covers. The checks must exercise quick covering,
SMT covering, and deletion. They must also check neutral-only applicability of
supplied invariants and approximations.

Regression runs must include safe and unsafe internal loops in both schedulers,
ordinary non-transaction cases, and `make test`. Every verifier run needs a timeout
and/or a node limit. An external wall-clock timeout must also bound runs until
tests establish that internal cubes count toward enforced limits. This safeguard
applies to child verifier processes and regression suites as well as focused runs.
Tests must explicitly check internal-node accounting and limit enforcement in
both schedulers. Reports must record the limits and distinguish conclusive
verdicts, Cubicle limit exits, external timeouts, and crashes.

Passing tests does not discharge the proof obligations. The design also makes no
termination claim for arbitrary transaction loops. Transaction certificate support
remains unverified.

Performance needs measurement. Shared control symbols do not add process witnesses
to permute, but active control-only witnesses can enlarge the instantiation
support. Experiments should compare explicit and specialized control reasoning
where feasible. They should measure the cost rather than assume that datatype
control is prohibitively expensive.

## 9. Recorded implementation status and historical evidence

The former step 9 recorded that experimental internal covering had been commented
out pending review. The fused `Fixpoint.Located` module and its `Covers` alias had
been removed. Search used `Cubetrie` storage and the `FixpointTrie` checker, with
the boundary policy in `bwd.ml`. Internal obligations had no covering, storage, or
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
