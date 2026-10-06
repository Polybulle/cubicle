# Bounded independent final correctness review

## Disposition

**Approve the reviewed implementation within spec r1's approved assumptions.** No unresolved correctness blocker found in this bounded review. This is independent source review plus short direct-probe execution, not a proof or task-completion decision. Tetra retains final acceptance authority.

Reviewed working-tree diff on branch `tetra`, HEAD `158f0feb9bdab462bb83f1ce18581b63e0e222d5`, against that baseline. Main workspace was read-only; no build, edit, commit, or long experiment was performed. The parent stated source edits were finished for review.

## Scope and assumptions

Read the governing specification and implementation proposal, project guide, actual production diff, tests/tx-fixpoint/README.md, check.ml and run.py, and relevant underlying trie, substitution, ancestry and certificate-consumer code. The review focused on the two previous findings and the surrounding normalization, joint covering, deletion and scheduler paths.

Accept without reopening the approved large-cardinality assumption: the population is sufficient for all names in each query. Exact smaller-population behavior and arbitrary-loop termination are excluded. Existing Cube/SMT/pre-image contracts, unique node tags, complete declared cube support and immutable node data/location after insertion remain prerequisites. Optional backend final rebuild evidence belongs to the parent.

## Correctness observations and attempted counterexamples

### Storage and ancestry

`Cubetrie.Located` now separates the compressed lookup index from retained transaction nodes. Insertion of a broad descendant may replace an ancestor in the index, but does not remove that ancestor from `bucket.nodes`. Both constructor-scoped hard covering and public enumeration use the retained list in transaction mode. Explicit deletion traverses that list rather than just visible index leaves; a changed bucket is rebuilt from its survivors.

I traced the dangerous sequences: narrower ancestor then broader child; removal of the broad child; replacement deleting an ancestor hidden behind the broad index entry; incompatible bindings/support sizes; and descendant cleanup at another constructor. The separate retained list, shared original objects and rebuild close the reported erasure hole. `Node.ancestor_of` checks original tags through history, and `Node.has_deleted_ancestor` follows the full history independently of location. Explicit subsumption preserves an ancestor of the new covering node unless it already has a deleted ancestor. Dependency cleanup is deliberately not constructor-restricted.

Internal quick checks/deletion require the same constructor, normalized active tuple and support size. Permutations act on data and tuple together. This is conservative: it can miss deletions, but does not delete an incompatible obligation merely because its data matches. Neutral data-only buckets preserve the approved large-population treatment. Ordinary non-transaction storage remains compressed as intended.

The direct regression checks the ancestor remains enumerated, becomes available after removal of the broad child, and receives deletion flags even when hidden by compression. It also checks cross-location cleanup. No remaining counterexample found in these paths.

### Certificate inverse renaming

`FixpointCertif.useful_instances` normalizes both goal and covers locally, then composes each cover's original-to-normalized map, its selected instance substitution, and a shared inverse goal map. Crucially, the inverse is extended once over the union of extra target names of all selected instances. Fresh images exclude every original goal name. Consequently distinct extra normalized names remain distinct, cannot collide with original gapped goal names, and the same extra name is restored consistently across different returned instances. Returned cover nodes are the original objects, found by their existing unique tags.

I considered the previous collision shape (goal original `#2` normalized to `#1`, with an instance also using normalized `#2`), several distinct extras, and extras shared between different cover instances. The shared extension prevents the collision; overlap between substitution domain and image is harmless because substitution is simultaneous/name lookup rather than transitive rewriting. Transaction extraction remains explicitly rejected.

### Covering and integration

Combined data/control support is normalized with one injective renaming and does not rewrite history. Internal alignment is a partial substitution of cover-bound variables, rejecting conflicting repeated arguments and noninjective images. Target support is duplicate-free and extended before removing already-fixed images, so enough names remain for every unfixed source variable. Zero-argument internal constructors still take exhaustive enumeration. Different constructors contribute no instances.

The joint check resets once, asserts goal/full-support distinctness once and accumulates all retained negated instances in that context. Under the approved cardinality assumption, a state outside all existential covers extends to a model of these ground negations with distinct query names. Therefore unsatisfiability is a sufficient covering test; neither completeness nor exact small-population semantics follows.

Both backward schedulers now cover/store internal nodes while retaining neutral guards around safety and approximation. Temporary parallel stores are accumulated after forming each task's prior snapshot. The production diff preserves the accounting, limit, predecessor and postponement paths for non-covered internal nodes.

## Evidence actually inspected or executed

- Scoped `git diff --check` on production changes and maintained affected tests: exit 0.
- Whole-tree `git diff --check`: reports trailing whitespace only in pre-existing user work `tx-fixpoint.md`; left untouched, not treated as an implementation defect.
- Parsed existing `tests/tx-fixpoint/.local/results.json`: 90 records, comprising 2 probe results, 48 SAFE, 27 UNSAFE and 13 expected limits. This is inspected parent-run evidence, not independently rerun integration coverage.
- Independently executed the existing compiled direct probe from the scratch directory, with absolute input path and 40-second per-process timeout, in both `-tx none` and `-tx bwd` modes. Both returned exit 0 and `PASS located fixpoint contracts`; transaction mode also reported `PASS 1000 finite-semantics covering comparisons` and `PASS scheduler storage and neutral-only policy`, with nodes 1 and 2. Expected caught initial-intersection checks print `Unsafe trace: unsafe[7]` on stderr in both modes; these are not failed probe verdicts.

Exact probe command pattern:

```
/Users/hector/cubicle/tests/tx-fixpoint/.local/check.opt -nocolor -nosubtyping -solver alt-ergo -j 0 -tx MODE /Users/hector/cubicle/tests/tx-fixpoint/model.cub
```

The probe was already compiled by the parent; this reviewer did not independently rebuild or establish binary/source freshness. Source findings above are grounded in the current files, not inferred from probe success.

## Nonblocking test gaps / limits

1. The certificate extra-name regression checks returned substitution injectivity and multiple instances, but does not explicitly assert shared extra-name consistency across instances or replay the entire returned instantiated union in the original goal namespace. The implementation supplies that consistency by one shared map; a dedicated semantic replay would better guard future regressions. No current defect identified.
2. The finite interpreter is genuinely independent of production normalization/instantiation, but covers a small Boolean-array fragment at population three. Repeated-argument and ancestry cases are separate direct tests, not broad randomized storage-operation sequences. No exhaustive graph/deletion or arithmetic proof is claimed.
3. I did not rerun make test, real Functory, optional Z3 builds, certificate generation/Why3 checking, or asynchronous scheduling stress. Parent evidence and its final isolated rebuilds must discharge the acceptance items outside this bounded review. The documented existing Z3 core-tracking and subtype-sort limitations remain explicit exclusions, not passing checks.
4. Retaining transaction nodes outside the compressed index trades memory/iteration cost for correctness. No performance bound or arbitrary-loop termination conclusion is justified.

## Files changed

Only this scratch review report was created. No main-workspace files were modified.
