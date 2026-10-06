# Tuple views (spec r4)

Implemented and reviewed directly by Tetra at Kes's request. Node.view is now
Cube.t * node_position. It carries no original node, duplicated support, or
renaming. Node.data_view returns Cube.t * Variable.subst for certificate naming;
other data-only callers discard the renaming. Prover.assume_cube_no_check takes
only the original tag and normalized cube. Located storage retains original nodes.

The normalizer still applies one substitution to data and control arguments.
Support comes from Cube.vars, including control-only variables. Permutation acts
on the cube and position together. Located.fold_at needs only the constructor,
so it reads the original validated position without constructing a normalized
query. Certificate extraction retains its original nodes alongside normalization
results, eliminating a redundant original-node list and lookup. The capture-free
shared inverse remains unchanged. The original checker control flow is retained.

## Verification

- make: passed; interfaces and callers rebuilt, executable relinked.
- make test: all 14 standard regressions passed.
- Focused suite: 93 sequential Alt-Ergo, 93 real two-core Functory, and 92 Z3
  executions passed. Fresh isolated builds used the existing switches.
- All 278 verdicts and exit codes match the overlay baseline by command arguments.
- All changed code files match the tested isolated copies byte-for-byte.
- Existing located-covering and neutral-candidates suites rebuilt and passed.
- Scoped git diff --check passed.
- Source search found no remaining old record-field accesses or assume_view_no_check.

The direct test was adapted to destructure the tuple and call the narrower prover
API; assertions and expected outcomes are unchanged. An initial test compilation
failed because a local cube value shadowed its cube-construction helper; renaming
that local value fixed the compilation. Production compilation passed initially.

Outputs: .local/tuple-sequential.json, .local/tuple-parallel.json,
.local/tuple-z3.json. Existing Z3 static-subtyping exclusions and large-cardinality
assumptions remain. Tests are regression evidence, not a proof of soundness or
arbitrary-loop termination. No new independent review or performance claim is made.
No commits were made.
