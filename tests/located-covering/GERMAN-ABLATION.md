# German internal-covering ablation

Baseline: `fe6170b318e8315f19937d18b0e8d7a251b5783f`, with no tracked working-tree
changes. Input: `examples/german_looped.cub`, unchanged. Both binaries were built
from the same isolated snapshot with OCaml 5.4.1 (opam switch `5.0.0`), using
Alt-Ergo and sequential search (`-tx bwd -j 0`), without approximation.

## Result

Internal covering is necessary for the observed successful completion of this
benchmark under the tested bounds. All six configurations return SAFE with it;
none return a verdict without it. This is a controlled experimental result, not
a proof that every possible search without internal covering diverges.

At `-nodes 1000`, with a 10-second external process-group timeout per run:

| Search | Postponement | Enabled | Disabled |
|---|---:|---|---|
| BFS | 0 | SAFE, 27 nodes | `Internal failure:Not enough procs`, last node 237 |
| BFS | 1 | SAFE, 24 nodes | External timeout, last printed node 291 |
| BFS | 2 | SAFE, 35 nodes | `Internal failure:Not enough procs`, last node 207 |
| DFS | 0 | SAFE, 396 nodes | `Internal failure:Not enough procs`, last node 9 |
| DFS | 1 | SAFE, 38 nodes | `Internal failure:Not enough procs`, last node 323 |
| DFS | 2 | SAFE, 109 nodes | `Internal failure:Not enough procs`, last node 33 |

At `-nodes 100`, enabled BFS and DFS/postponement 1 return SAFE with the same
counts. Enabled DFS/postponement 0 and 2 reach the limit. Disabled BFS and
DFS/postponement 1 reach the limit; the other disabled DFS runs fail as above.
Cubicle's limit check allows 101 visited nodes at this setting.

Disabled traces repeatedly prepend `inv_shr` with additional process variables.
For example, DFS/postponement 0 reaches:

```text
inv_shr(#10) -> inv_shr(#9) -> inv_shr(#8) -> inv_shr(#7) ->
inv_shr(#6) -> inv_shr(#5) -> inv_shr(#4) -> gnt_excl(#3) -> unsafe[1]
```

Each backward invalidation can restore another prior sharer. Boundary covering
alone does not close these internal branches in these runs. The process-variable
failure is an implementation limitation exposed by the ablation, not a safety
verdict. Neither it nor the timeout establishes mathematical nontermination.

## Exact intervention

Only the sequential scheduler's covering call in the isolated `bwd.ml` changed:

```diff
-          match Fixpoint.check norm !visited with
+          match (if at_boundary system n then Fixpoint.check norm !visited
+                 else None) with
```

Safety, boundary covering, internal storage, deletion, predecessor generation,
and scheduling remain unchanged. The parallel scheduler was not modified or
tested. Production sources and the main executable were not changed.

## Reproduction and evidence

Build a fresh snapshot using the existing helper (explicit scratch path):

```sh
python3 tests/tx-fixpoint/build-isolated.py \
  /Users/hector/.hermes/profiles/tetra/cache/scratch/german-covering-ablation-new \
  --switch 5.0.0
```

Save its `cubicle.opt` as `covering-enabled.opt`, apply the above change to the
snapshot's `bwd.ml`, and rebuild there with
`/opt/homebrew/bin/opam exec --switch=5.0.0 -- make`. Both builds must show a link
step. Then run from the main checkout, substituting the snapshot paths:

```sh
python3 tests/located-covering/german-ablation.py \
  --enabled SNAPSHOT/covering-enabled.opt --disabled SNAPSHOT/cubicle.opt \
  --nodes 100 --output tests/located-covering/.local/german-ablation.json
python3 tests/located-covering/german-ablation.py \
  --enabled SNAPSHOT/covering-enabled.opt --disabled SNAPSHOT/cubicle.opt \
  --nodes 1000 --output tests/located-covering/.local/german-ablation-1000.json
```

The two local JSON reports retain all 24 commands, raw outputs, exit statuses,
classifications, model hash, binary hashes, and baseline revision. The actual
snapshot is `/Users/hector/.hermes/profiles/tetra/cache/scratch/german-covering-ablation`;
it is temporary, not a durable dependency. The ablation runner reports failures
rather than treating them as SAFE or asserting a universal expected outcome.
No production code changed; a full `make test` was not run for this experiment.
