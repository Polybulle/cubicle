# Benchmark reports

Versioned source for the dependency-free benchmark dashboard and repetition-spread
review. Raw benchmark archives and generated HTML remain outside the checkout;
these tools do not alter recorded measurements or exclude outliers.

From the repository root, with an existing archive directory:

    python3 experiments/report/analyze_repetitions.py --source /path/to/cubicle-bench/run-08
    python3 experiments/report/build_dashboard.py --source /path/to/cubicle-bench/run-08 --output /path/to/cubicle-bench/run-08.html

Keep generated dashboard pages in the parent directory of their run archives so
relative input, log, and provenance links resolve. The dashboard includes all
recorded configurations, including `tetra-fwd` and `old-fwd`. Optional
`--overlay /path/to/run-06 /path/to/run-07` preserves the historical overlay
workflow; overlays must share their parent archive directory and matching inputs
and recipes. Do not overlay the repaired Flash input onto its historical model.

The spread review needs three completed repetitions per cell. It flags an isolated
slow sample when it is at least twice the median of the other two and at least
0.1 seconds slower. It flags longer-run variation when the median is at least one
second and max/min is at least 1.10. These are review heuristics, not significance
tests. The report uses the archive's actual benchmark-worker count.

## Tests

    python3 -m unittest discover -s experiments/report -p 'test_*.py' -v

Repetition tests are self-contained. Dashboard integration tests use the retained
run-01, run-05, run-06, run-07, and run-08 archives; set their parent directory to
exercise them (otherwise the archive-dependent tests are skipped):

    CUBICLE_BENCH_ARCHIVE=/path/to/cubicle-bench python3 -m unittest discover -s experiments/report -p 'test_*.py' -v

For run-08 all 15 tests passed with the retained archives. Browser checks covered
forward-mode cells and labels, their graph ratio, record details, CSV export,
served log access, and mobile layout. Publication must include the run's linked
metadata, `inputs/`, and `logs/`, not only the HTML page. Verify served files over
HTTP after copying; do not publish compiled builds or unrelated source trees.

## Sequential baseline

Run-08 freshly measured 28 models and 128 model/configuration pairs with one worker
and three repetitions per completing pair. There were 370 measured executions:
363 completed, six timed out, and one was unsupported. No measurements were reused.
The current build includes the forward-engine fixes and restored Flash annotations.
Flash uses `-tx fwd -brab 2` without a depth limit and completed SAFE in all repeats.

Report: https://static.home.hectorsuzanne.com/cubicle-bench/run-08.html

Spread review: https://static.home.hectorsuzanne.com/cubicle-bench/run-08/outliers.html

The raw archive retains source/input/binary hashes, commands, counters and logs.
Two first-invocation spikes are flagged, with unchanged search counters; no samples
were removed. The prior report and historical archives remain unchanged.
