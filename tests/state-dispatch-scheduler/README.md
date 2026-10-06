# CFG dispatch and scheduler regressions

Run from the repository root in the configured OCaml environment:

```sh
make
make -f Makefile -f tests/state-dispatch-scheduler/probe.mk isoqa-probe
python3 tests/state-dispatch-scheduler/run.py --matrix --output tests/state-dispatch-scheduler/.local/current-matrix
python3 tests/state-dispatch-scheduler/run-probes.py --output tests/state-dispatch-scheduler/.local/current-probes
```

Each output directory must be new. Both runners bound verifier process groups
and retain commands, native exit codes, and complete output. The default is
sequential; add `--parallel` explicitly to include `-j 2` when linked against
real Functory. The configured fake library cannot exercise those cases.

The probes cover internal-call binding, explicit final-call dispatch, release
with shared witnesses/history, entry/internal splitting, executable trace depth,
and scheduler separation of boundary/internal nodes. The matrix checks SAFE,
UNSAFE, search strategies, postponement, deletion, and ordinary-mode behavior.
The finite internal cycle is now expected SAFE with located covering; genuine
nonconvergence and resource-limit cases are retained in `tests/tx-fixpoint`.

The BRAB instrumentation records both internal and final-call dispatch:

```sh
make -f Makefile -f tests/state-dispatch-scheduler/review.mk isoqa-review-approx
tests/state-dispatch-scheduler/.local/review-approx.opt -tx all -brab 2 -forward-depth 6 -nodes 5000 -quiet examples/german.cub
```

An ordinary model may have no internal dispatch after entry contraction. Historical
review documents and measurement files describe the earlier implementations and
are not the current acceptance specification.
