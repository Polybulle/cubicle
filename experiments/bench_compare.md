# Curated five-configuration benchmark

`bench_compare.py` builds and runs a curated corpus. It never checks out a branch,
changes the current working tree, fetches, commits, or modifies model contents.
The default command only prints a plan.

## Scope

`bench_manifest.json` records the pinned commits, model-specific options and their
source evidence, verdict expectations where documented, caveats, and provisional
pilot ceilings. The audit covered all 132 tracked `.cub` files: 79 top-level and
53 challenge files. The initial selection contains 28 models: 24 ordinary and
4 transactional. The other 104 are listed as excluded from this initial selection,
not as failed verification runs. The selection is editable and not a claim that
these are the only worthwhile models.

Ordinary inputs run on baseline Cubicle (`origin/master`) and on both transaction
settings (`none`, `all`) of each fork (`fork-hector/tetra`, `fork-hector/master`).
Transactional inputs run unchanged only on the two `all` configurations, as Kes
approved. They need no unannotated counterpart. All inputs come from the pinned
Tetra corpus commit, not from each build's branch-specific example directory.

Each model has one recipe rather than an automatic backward/BRAB-2 cross-product.
The regression recipes take precedence for `flash_nodata.cub` and
`flash_buggy.cub`: forward depth 6, while their headers suggest 5. Evidence and
conflicts are retained in the manifest. The transaction Flash model uses its
own header recipe, depth 5. `ticket_o.cub` uses BRAB-0 and numerical abstraction
0..2. HIRR uses BRAB-4, `bfsh`, candidate heuristic 2, and forward depth 31.
The three German transaction inputs have no established model-specific flags;
plain backward reachability is explicitly labeled a proposed pilot recipe.

Some ordinary Flash models have duplicate transition names that the transaction
implementations may reject. They remain useful compatibility checks; the runner
does not rename transitions to manufacture successful runs.

## Commands

Requirements: Python 3, git, tar, autoconf, make, and an active OCaml/opam
installation with `num` and `ocamlfind`. Activate the intended opam environment
before execution. Hyperfine is not required.

Inspect the selected invocations without building:

    python3 experiments/bench_compare.py --phase plan

Build all three revisions, then typecheck and calibrate the selected models:

    python3 experiments/bench_compare.py --phase pilot --output "$HOME/cubicle-bench/pilot-01"

Run pilots followed by three measured repetitions of every completing cell:

    python3 experiments/bench_compare.py --phase run --runs 3 --output "$HOME/cubicle-bench/run-01"

A small end-to-end batch:

    python3 experiments/bench_compare.py --phase run --runs 2 --models bakery.cub bakery_lamport_bogus.cub german_looped.cub --output "$HOME/cubicle-bench/smoke-01"

The output directory must not exist and must be outside the source checkout.
Every invocation rebuilds from clean git archives; there is no resume/cache mode.
A durable directory such as `$HOME/cubicle-bench` is preferable for actual results.
Scratch directories used during runner development are temporary.

Run harness tests:

    python3 -m unittest discover -s experiments -p test_bench_compare.py -v

## Builds and execution

All builds use the same compiler environment and built-in Alt-Ergo. The make
invocation explicitly includes the installed `num` directory and `+unix` for
all three revisions. This is necessary because baseline Cubicle's old Makefile
assumes the earlier OCaml library layout. The version string is the pinned commit,
so the archived build does not need a `.git` directory. Build commands/logs and
binary hashes are recorded. No algorithm source is patched.

Every command uses `-quiet -nocolor -solver alt-ergo`. Other search limits remain
at their branch defaults; the inspected revisions currently share the usual
process/depth/node defaults. The manifest's model-specific flags are the same
across configurations. Each cell first undergoes a 30-second type-only check.
Unsupported syntax/features are not timed as successful model checking.

Pilots escalate only on wall-clock timeout. Provisional ceilings are 30/120 seconds
for short models, 30/120/600 for medium ones, and 120/600/1200 for the two HIRR
models. These are exploration budgets, not measured predictions. Internal search
limits, errors, and unsupported features do not trigger automatic flag changes.
The largest pilot budget attempted for a model becomes the common timeout for
its measured repetitions. Non-completing cells remain in the report but are not
repeated as timing benchmarks.

Execution is sequential. A seeded shuffle chooses configuration order during
pilots and model/configuration order in each measured round. Pilots are not
included in timing summaries. There is no additional warmup, cache flushing, or
CPU-affinity control. Wall time includes process launch and a small timer-management
overhead. A blocking wait avoids Python's timed-wait polling distortion on short
runs. Complete output and verdicts are checked for every measured repetition.

Hyperfine is installed and may be useful for a later precision pass on very short
models. This first runner uses direct timing to retain per-run output, verdicts,
and independent timeouts without a timed Python wrapper inside hyperfine. Do not
interpret small differences on millisecond-scale models as established speedups.

## Outputs and interpretation

- `manifest.json`, `environment.json`: scope, pinned revisions, options and seed.
- `*-build-commands.json`, `logs/*-make.log`: build provenance and diagnostics.
- `binary_hashes.json`, `inputs.json`: executable and exact input hashes.
- `pilot.json`: final preflight/pilot outcomes and model-specific timing budgets.
- `runs.jsonl`, `runs.csv`: incremental raw records, commands, verdicts and timings.
- `summary.json`: measured counts, median times, failure classes, verdict disagreement
  and mismatch against documented expectations. Raw JSON records also retain
  available Cubicle statistics.
- `logs/`: merged stdout/stderr for each build, preflight, pilot and measured run.

SAFE and UNSAFE are distinct successful verification outcomes. A nonzero UNSAFE
exit is not automatically a command failure. Crashes, input errors, unsupported
features, limits/unknown, missing verdicts, and timeouts remain separate outcomes.
A timeout is not replaced by its time limit in a successful-runtime average.

`timing_eligible` requires all measured repetitions for that cell to complete,
no observed verdict disagreement for the model, and no mismatch against its
documented expected verdict. A missing/unsupported competitor is not a speedup.
The runner does not compute cross-model speedup averages or claim soundness from
matching verdicts. Pilot-only output intentionally has no measured medians.

## Verification performed during development

All three pinned revisions built successfully with OCaml 5.0.0. The end-to-end
smoke batch checked and timed `bakery.cub`, `bakery_lamport_bogus.cub`, and
`german_looped.cub`. Both ordinary models completed on all five configurations
with the expected SAFE and UNSAFE verdicts. Tetra-all returned SAFE for the looped
model; old-all rejected its trigger cycle during typechecking. The final smoke
batch contains 12 typechecks, 11 completing pilots and 22 measured runs (two per
completing cell). These validate the harness, not a full comparative benchmark.
