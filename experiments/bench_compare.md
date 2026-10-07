# Curated five-configuration benchmark

`bench_compare.py` builds and runs a curated corpus. It never checks out a branch,
changes the current working tree, fetches, commits, or modifies model contents.
The default command only prints a plan.

## Scope

`bench_manifest.json` records the pinned commits, model-specific options and their
source evidence, verdict expectations where documented, caveats, and provisional
timeouts calibrated from `cubicle-bench/run-01`. The audit covered all 132 tracked `.cub` files: 79 top-level and
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

## Reusing unchanged comparison results

Add `--reuse-from /path/to/run-02` to reuse baseline and old-branch results.
The runner matches pinned commits, input hashes, command options and time budgets;
Tetra always runs afresh. Missing or incompatible cells run normally. Builds with
no remaining cells are skipped. Reused records keep their original commands,
phases and log links, with `reused_from` and a `reuse.json` inventory. Historical
pilot failures and unsupported typechecks remain failures in their original phases,
not invented measured timings. Keep the source archive available.

These are historical comparisons, not contemporaneous measurements: in particular,
sequential archived timings and new four-worker timings may differ due to contention.

## Commands

Requirements: Python 3, git, tar, autoconf, make, and an active OCaml/opam
installation with `num` and `ocamlfind`. Activate the intended opam environment
before execution. Hyperfine is not required.

Inspect the selected invocations without building:

For a focused rerun, a manifest model may specify a nonempty `configs` list,
for example `["tetra-all"]` or `["tetra-none"]`. Only those configurations are
scheduled for that model. Duplicate, unknown, or unsupported configurations are
rejected; omitting the field preserves the original five-configuration matrix.
Supply the focused manifest with `--manifest /path/to/manifest.json`.

    python3 experiments/bench_compare.py --phase plan

Build all three revisions without running models:

    python3 experiments/bench_compare.py --phase build --output "$HOME/cubicle-bench/build-01"

Run up to three measured repetitions per cell, starting directly with measurement:

    python3 experiments/bench_compare.py --phase run --runs 3 --output "$HOME/cubicle-bench/run-01"

A small end-to-end batch:

    python3 experiments/bench_compare.py --phase run --runs 2 --models bakery.cub bakery_lamport_bogus.cub german_looped.cub --output "$HOME/cubicle-bench/smoke-01"

The output directory must not exist and must be outside the source checkout.
Every invocation rebuilds in isolation; there is no resume/cache mode.
Pass `--working-tree` to include current uncommitted changes in the Tetra build
and shared model inputs. The runner snapshots tracked and nonignored untracked
files under `source/`, excluding generated/ignored artifacts. It records the
base commit, working-tree status, binary diff, and per-file SHA-256 hashes in
`working-tree.json` and `working-tree.patch`. Baseline and old builds still use
their pinned Git archives. All configurations receive the same snapshot inputs.
Without this option, builds and inputs use the pinned commits in the manifest.

    python3 experiments/bench_compare.py --working-tree --phase run --runs 3 --output "$HOME/LMF/cubicle-bench/run-02"
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

Every command uses `-nocolor -solver alt-ergo`, retaining statistics and traces
in its log. Timing includes this output. Other search limits remain
at their branch defaults; the inspected revisions currently share the usual
process/depth/node defaults. The manifest's model-specific flags are the same
across configurations. There are no separate typechecking or pilot phases.
Unsupported syntax/features are classified from the measured attempt, never as
successful model checking.

Every scheduled cell starts at its final model-specific timeout, with no
escalation or timeout retry. Budgets are 5s for 20 short models; 100s for
`bakery_lamport_na`, `sense_barrier`, and `flash_abstr`; and 450s for
`chandra_toueg`, `flash_nodata_tx`, both HIRR models, and
`flash.ctc_home2_sort_pred`. These use the smallest tier covering the first run's
successful executions, with 450s for cells that still timed out at their final
pilot ceiling. The evidence is recorded per model in the manifest. Past input
errors are not grounds for skipping repaired inputs or the new build.
The same timeout applies across all configurations and measured repetitions of
a model. Internal search limits, errors, and unsupported features do not trigger
automatic flag changes. Repetition 1 counts toward the requested measured runs,
not as a discarded warmup. A non-completing attempt remains in the report and
stops further repetitions for that cell, including failures in later rounds.

Execution defaults to one worker and supports up to six. `--jobs 6` multiplexes up to six Cubicle
processes, with a new job dispatched whenever a slot becomes free. Every command
explicitly ends its options with `-j 1`, selecting sequential Cubicle execution
in all three revisions. This is six independent model-checking runs, not
Cubicle's parallel search mode. Builds finish before any benchmark is launched.

For run 03, after run 02 has finished:

    python3 experiments/bench_compare.py --working-tree --jobs 4 --phase run --runs 3 --output "$HOME/LMF/cubicle-bench/run-03"

macOS manages CPU placement: there is no strict core pinning or reservation.
Four sequential processes can still contend for caches, memory bandwidth, and
CPU resources. The environment record includes worker count, Cubicle core count,
and the lack of pinning; the HTML report displays the concurrency warning in its
timing description. Do not treat run 03 wall times as interchangeable with the
sequential run 02 timings. The 5/100/450s budgets remain wall-clock limits.

Measured rounds do not overlap, so two repetitions of a cell cannot run
concurrently. A seeded shuffle chooses model/configuration submission order
in each measured round; completion order is not deterministic. Timeout
clocks begin inside workers, excluding queue time. Each child has its own process
group, deadline, and log. Only the coordinator writes JSONL records.
On interruption, no further jobs are submitted; active jobs finish or reach their
deadlines before the worker pool exits.

Stdout shows progress bars for build steps and measured
rounds, with the job being dispatched.
Bars update in place in a terminal and use separate lines when redirected.
Progress counts executions within each phase or measured round.
There is no separate warmup, cache flushing, or
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
- `runs.jsonl`, `runs.csv`: incremental raw records, commands, verdicts and timings.
- `summary.json`: measured counts, median times, failure classes, verdict disagreement
  and mismatch against documented expectations. Raw JSON records also retain
  available Cubicle statistics.
- `logs/`: merged stdout/stderr for each build and measured run.

SAFE with exit code 0 and UNSAFE with exit code 1 are distinct successful
verification outcomes. A verdict with any other exit code is an error, not a
successful timing. Exit code 1 alone does not establish UNSAFE.
Crashes, input errors, unsupported
features, limits/unknown, missing verdicts, and timeouts remain separate outcomes.
A timeout is not replaced by its time limit in a successful-runtime average.

`timing_eligible` requires all measured repetitions for that cell to complete,
no observed verdict disagreement for the model, and no mismatch against its
documented expected verdict. A missing/unsupported competitor is not a speedup.
The runner does not compute cross-model speedup averages or claim soundness from
matching verdicts. Historical archives may contain typechecks and pilots; those
remain excluded when summarizing old runs.

## Historical verification before direct-measurement scheduling

All three pinned revisions built successfully with OCaml 5.0.0. The end-to-end
smoke batch checked and timed `bakery.cub`, `bakery_lamport_bogus.cub`, and
`german_looped.cub`. Both ordinary models completed on all five configurations
with the expected SAFE and UNSAFE verdicts. Tetra-all returned SAFE for the looped
model; old-all rejected its trigger cycle during typechecking. The final smoke
batch contains 12 typechecks, 11 completing pilots and 22 measured runs (two per
completing cell). These validate the harness, not a full comparative benchmark.
