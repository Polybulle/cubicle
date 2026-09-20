# Neutral-only candidates (step 8)

These tests exercise the production modules through their public interfaces and
small end-to-end models. They do not depend on the pre-existing untracked suites.

## Run

From the repository root, with the configured OCaml environment:

```sh
make
make -f Makefile -f tests/neutral-candidates/check.mk neutral-candidates-check
python3 tests/neutral-candidates/run.py
TEST_CORES=2 python3 tests/neutral-candidates/run.py # requires real Functory
timeout --kill-after=5 180 make test
```

Every focused verifier process has a 10-second external wall-clock timeout and a
node limit. The Python runner kills its process group on timeout. The Make suite
has its existing 30-second per-model external timeout; the command above also
bounds the whole suite. Keep external timeouts while parallel accounting remains
unverified.

## Contracts

`check.ml` checks supplied-invariant positions and initial assumptions, direct
approximation calls, search admission, and rejection/restart handling. It checks
that internal obligations matching a supplied invariant are still expanded.
It exercises rejection of an initially reachable approximation, preservation of
an unreachable survivor, resetting the survivor's deletion flag, and a subsequent
search with that survivor. It also checks the original/approximation distinction
and the original-formula match in candidate rejection. These restart checks call
the production search and rejection interfaces explicitly; they do not inject a
candidate into the private BRAB restart loop.

The models cover:

- `internal-invariant-unsafe.cub`: X temporarily violates its supplied invariant;
  X is restored before yielding, but Y reaches the boundary unsafe condition.
- `internal-invariant-safe.cub`: the corresponding entry requires unreachable
  Y = Bad, so the boundary property is safe.
- `no-entry-safe.cub` and `no-entry-unsafe.cub`: triggered-only systems, including
  a zero-step counterexample.
- `initial-invariant.cub`: an intentionally inconsistent trusted invariant
  excludes the initial cube. SAFE is expected because supplied invariants are
  assumptions, not assertions proved by Cubicle.
- `internal-cycle.cub`: an internal loop must stop at the node limit, without a
  SAFE or UNSAFE verdict. The current strict `>` limit check visits 21 nodes for
  `-nodes 20`.

All acyclic models run with postponement strategies 0, 1, and 2. Contract probes
run with `-tx bwd` and `-tx none` to check preservation of ordinary behavior.

## Observed results and gaps

The build, focused runner, and `make test` passed on the step-8 working tree.
Sequential internal-node accounting reached Cubicle's node limit before the
external timeout. This is experimental evidence for that path, not a termination
or soundness proof.

The main checkout has an empty FUNCTORYLIB and links fake_functory. An externally
bounded `-j 2` run there reported
`Internal failure:The functory library is not installed`.

An `opam install functory --dry-run` proposed replacing OCaml 5.4.1 with 4.14.4
and changing many existing packages. No installation was performed. Instead, the
existing 4.12.0 switch already contained Functory and Num. A separate temporary
source copy was configured and built with `opam exec --switch=4.12.0 -- ...`.
The full focused runner passed there with `TEST_CORES=2`, including contracts in
both transaction modes, all acyclic verdicts, and internal-cycle node accounting.
The main checkout's compiler, configuration, and binary were not replaced.

The German model also returned SAFE with `-tx bwd -brab 2 -nodes 5000`, under a
30-second external timeout. This complements the direct rejection/restart tests.

Certificate generation is not validated by this suite. The existing Why3
printers in trace.ml emit supplied invariants as unlocated axioms; transaction
certificate semantics require separate review before claiming those certificates
respect boundary-only invariants.
