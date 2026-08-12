# Transactional MCMT TODO

## Implement caller-local underscore resolution

**Status:** implemented and regression-tested in the 2026-08-12 working tree;
normalization coverage is documented in `kb/transactions.md`. Full correctness
of the transaction-aware analyses is not claimed.

### Required semantics

After named-argument normalization of a trigger path, retain one distinct
placeholder for every syntactic `_` occurrence.

An underscore occurs in an outgoing call of a caller transition `tᵢ(vᵢ)` and
supplies an argument of `tᵢ₊₁`. Its chosen process must be absent from the
already resolved active process arguments of its caller:

```text
value(u) ∉ values(vᵢ)
```

for each underscore placeholder `u` introduced by that outgoing call. The
several underscores introduced by one outgoing call must resolve to pairwise
distinct processes. They become actual arguments of the callee and preserve
their identity wherever ordinary named-call normalization subsequently
propagates them.

Underscore placeholders are **not globally fresh**. Separate occurrences may
alias when the caller-local constraints of both occurrences permit it. In a
long transaction path, the same process may therefore fill two different `_`
occurrences.

### Example

```text
t₁(x, y, z) calls t₂(x, _, _)
t₂(a, b, c) calls t₃(a, _, _)
```

Normalizes to the skeleton:

```text
t₁(x, y, z) → t₂(x, u₁, u₂) → t₃(x, u₃, u₄)
```

The required constraints are:

```text
u₁, u₂ ∉ {x, y, z};  u₁ ≠ u₂
u₃, u₄ ∉ {x, u₁, u₂};  u₃ ≠ u₄
```

Thus `u₃` or `u₄` may equal `y` or `z`, but neither may equal `x`, `u₁`, or
`u₂`.

### Engineering requirements

1. Preserve the source location and caller transition associated with every
   underscore occurrence until its caller-local exclusion has been handled.
2. Reuse Cubicle's existing fresh-under-condition/relevant-instance machinery
   where it matches this contract; trace its definition, callers, and
   distinctness guarantees before editing transaction code.
3. Do not enforce global pairwise distinctness of all underscore occurrences.
4. Preserve a propagated underscore's selected identity through later named
   calls.
5. Add discriminating regressions for permitted aliasing and forbidden
   caller-local aliasing, with both transaction-aware forward and backward
   paths where applicable.
6. Prove or document the relevant-instance coverage argument before claiming
   correctness of the implementation.

## Deliberately deferred

- New work on `part`: its current serial-permutation semantics is accepted, but
  the feature is a candidate for removal.
- Reworking the forward candidate-filtering heuristic: intermediate transaction
  states remain intentionally recorded for negative candidate filtering; see
  `kb/transactional-mcmt.md`.
