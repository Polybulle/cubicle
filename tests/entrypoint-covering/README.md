# Entry-only covering

Skip covering at `Before t(args)` only when the CFG has exactly one predecessor
for `t`, namely `neutral`. Backward expansion then releases the control bindings
and immediately returns to a boundary cube, where normal covering still runs.
This does not remove the internal node or change the modeled transitions.

An entrypoint may also be called internally. Such a location must retain
covering, especially if it participates in a cycle. The fixture distinguishes:

- an ordinary transition and an entry-only transition that starts a transaction;
- a triggered internal loop and an entrypoint that recursively calls itself;
- the neutral boundary, which must still be checked.

The unit check also supplies an existing cover for an entry-only node and checks
that the scheduler expands it instead of covering it. The integration matrix
checks recursive-entry convergence in both transaction modes, both search orders,
and all three postponement strategies, with an external timeout and node bound.

```sh
make
make -f Makefile -f tests/entrypoint-covering/check.mk entrypoint-covering-check
python3 tests/entrypoint-covering/run.py
```

With a real Functory build, use `TEST_CORES=2` for the same runner. Keep builds
for different OCaml switches in separate source copies.

Validation of this change also ran the located-covering and neutral-candidates
suites sequentially and with two workers, and the main `make test` target.
The existing located-covering unit test uses synthetic locations, so its synthetic
CFG explicitly enables their covering rather than querying the model's CFG.

On the four original benchmark inputs, sequential `-tx all` solver calls changed:

| Model | Before | After |
|---|---:|---:|
| bakery_lamport_na | 76,594 | 1,689 |
| szymanski_na | 32,846 | 189 |
| sense_barrier | 458,801 | 5,635 |
| ricart_abdulla_int | 35,465 | 61 |

All four remain SAFE; `-tx none` counters are unchanged. Skipping covering
increases expansion counts, so the performance improvement is in solver work,
not fewer search nodes. Raw post-fix records are the `entrypoint-fixed` entries
in the benchmark investigation's `probes.jsonl`.
