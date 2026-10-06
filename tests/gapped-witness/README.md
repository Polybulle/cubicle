# Gapped-witness and control-binding regressions

Run from the repository root after building:

```sh
make
python3 tests/gapped-witness/run.py
```

The runner checks verdicts and native exit codes under process-group timeouts.
It retains execution records in `.local/results.json`.

## Expected results

- `unsafe.cub`: UNSAFE; distinct processes can reach the second unsafe clause.
- `safe.cub`: SAFE; no transition sets an A entry true.
- `control-safe.cub`: SAFE with transaction backward exploration, UNSAFE with
  `-tx none`, where the low-level transitions execute independently.
- `control-unsafe.cub`: UNSAFE; the swapped call chain selects the process whose
  A entry was set.

The runner varies transaction mode, search strategy, and subtyping/deletion. Its
independent finite interpreter checks populations 1–5 for the original pair and
1–3 for the control-binding pair. These checks do not establish general soundness.

The old direct constructor probe has been removed: it required `Node.create` to
normalize eagerly and used a superseded trie interface. Current covering and
normalization contracts are exercised by `tests/tx-fixpoint`; boundary release,
shared witnesses, and history are exercised by `tests/cfg-boundaries` and
`tests/state-dispatch-scheduler`.
