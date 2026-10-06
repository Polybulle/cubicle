# CFG boundary API

Run from the repository root in the OCaml environment:

```sh
make
make -f Makefile -f tests/cfg-boundaries/check.mk cfg-boundaries-check
```

Expected: PASS in both `-tx all` and `-tx none`. Uses the entrypoint-covering
fixture to check initial/final classifications, internal-only adjacency, finite
initial instantiation, and splitting an initial event with an internal parent.
The split must share the cube and history and retain internal covering.

Forward differential checks (including yielding with continuation):

```sh
make -f Makefile -f tests/forward-transactions/check.mk forward-transactions-check
python3 tests/forward-transactions/run.py
```
