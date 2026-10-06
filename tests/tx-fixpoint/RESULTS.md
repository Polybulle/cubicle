# Located fixpoint verification report

Scope: original implementation (spec r1), accepted by Tetra after implementation
checks and independent final review. The subsequent dynamic storage-selection
amendment is recorded separately in [SELECTION.md](SELECTION.md).
Baseline: `158f0feb9bdab462bb83f1ce18581b63e0e222d5`, working branch `tetra`.
No commits were made. Pre-existing modifications to `tx-fixpoint.md` and unrelated
untracked files were not edited.

## Executed checks

- `make depend` and `make`: passed.
- `make test`: passed.
- The exit-status-aware regression runner independently checked all 14 standard
  examples: 14 passed, zero failures, including SAFE/0 and UNSAFE/1 validation.
- `make -f Makefile -f tests/tx-fixpoint/check.mk tx-fixpoint-check` and
  `python3 tests/tx-fixpoint/run.py`: passed on the main OCaml 5.4.1 / Alt-Ergo build.
- Fresh isolated OCaml 4.12.0 / real Functory build: the same suite passed with
  `--cores 2`.
- Fresh isolated OCaml 5.4.1 / Z3 build: the suite passed with `--solver z3`, subject
  to the existing backend exclusions documented below.
- Existing `tests/located-covering` and `tests/neutral-candidates` probes and Python
  runners: passed sequentially and with real Functory / `TEST_CORES=2`.
- Scoped `git diff --check` on modified implementation and existing test paths:
  passed. Full-tree warnings are pre-existing whitespace in `tx-fixpoint.md`.

The focused JSON reports were read back and counted programmatically:

| Build | Probe executions | SAFE | UNSAFE | Expected node limits | Total |
|---|---:|---:|---:|---:|---:|
| Main Alt-Ergo, sequential | 2 | 48 | 27 | 13 | 90 |
| Real Functory, two cores | 2 | 48 | 27 | 13 | 90 |
| Z3, sequential | 2 | 48 | 26 | 13 | 89 |

All 269 focused executions matched their expected classifications. None crashed
or reached the external timeout. The direct transaction probe compares 1,000
located-covering cases against an independent finite interpreter in each build.
These are the same cases across builds, not 3,000 distinct cases.

Exact commands, exit statuses and outputs are retained in ignored local files:
`tests/tx-fixpoint/.local/results.json`, `parallel-final.json`, and `z3-final.json`.
The source and probe files in both final isolated builds were compared byte-for-byte
with the main checkout and matched.

## Review-driven corrections

The first independent review reproduced an ordinary-certificate inverse-renaming
collision and loss of a protected ancestor during insertion. Both have regressions.
Certificate restoration now uses a common injective inverse extended to fresh extra
names. Transaction storage now retains original obligations separately from its
compressed lookup index. Only explicit deletion removes those obligations, and
it sees obligations hidden by the index.

The ancestor regression was observed failing before the storage correction and
passing afterward. It also checks that removing the broader cover restores the
ancestor to lookup, and that explicit deletion reaches obligations hidden by index
compression. The [independent final review](REVIEW.md) approved the implementation
under spec r1's assumptions, with no unresolved correctness blocker. The reviewer
inspected the current source and independently ran the direct probe in ordinary
and transaction modes. Tetra accepts that review together with the execution
evidence above. The review's nonblocking test gaps remain explicitly recorded;
approval is not a machine-checked proof or an arbitrary-loop termination claim.

## Assumptions and exclusions

The approved large-cardinality assumption remains essential. No exact claim is made
for smaller populations, nor any termination claim for arbitrary loops. The
nonconvergent arithmetic model intentionally reaches the node limit. Passing finite
checks is not a proof of unbounded correctness. Local arguments and inherited
Cube/SMT/pre-image assumptions are stated in README.md.

Z3's existing wrapper uses untracked assertions and can return empty unsat cores.
SMT core-tag and certificate-instance assertions are therefore checked with Alt-Ergo,
not counted as covered on Z3. The existing Z3 static-subtyping path also produced
`enumeration sort name is already declared`; Z3 tests use `-nosubtyping` and exclude
those default-subtyping variants. Its isolated build explicitly links Zarith. No
backend wrapper or main-checkout configuration was changed.

Transaction certificates remain explicitly unsupported. Parallel Z3 was not tested.
No broad performance benchmark or universal completeness proof is claimed.
