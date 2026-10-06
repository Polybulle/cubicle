#!/usr/bin/env python3
"""Bounded baseline comparison; timings are observations, not a performance gate."""
import argparse
import json
from pathlib import Path
import re
import runpy
from statistics import median
from time import perf_counter

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
run = runpy.run_path(str(HERE.parent / "neutral-candidates/run.py"))["run"]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path)
    args = parser.parse_args()
    binaries = {"baseline": str(args.baseline.resolve()), "fixed": str(ROOT / "cubicle.opt")}
    rows = []
    cases = [
        ("reported-unsafe", [str(HERE / "unsafe.cub")], 1),
        ("initial-safe", ["-nosubtyping", str(HERE / "safe.cub")], 0),
        ("bakery", [str(ROOT / "examples/bakery.cub")], 0),
        ("swimming-pool", [str(ROOT / "examples/swimming_pool.cub")], 1),
        ("control-safe", ["-tx", "bwd", str(HERE / "control-safe.cub")], 0),
        ("control-unsafe", ["-tx", "bwd", str(HERE / "control-unsafe.cub")], 1),
        ("bounded-internal-cycle", ["-tx", "bwd", "-nodes", "2000", "-depth", "10000",
                                   str(HERE.parent / "located-covering/swap-safe.cub")], None),
    ]
    for case, options, expected in cases:
        samples = {label: [] for label in binaries}
        for repetition in range(5):
            labels = list(binaries) if repetition % 2 == 0 else list(reversed(binaries))
            for label in labels:
                command = [binaries[label], "-nocolor", "-quiet", *options]
                start = perf_counter()
                code, output = run(command, seconds=30)
                seconds = perf_counter() - start
                verdict = ("LIMIT" if "Reached Limit !" in output else
                           "UNSAFE" if "UNSAFE" in output else
                           "SAFE" if "The system is SAFE" in output else "ERROR")
                nodes = re.search(r"Number of visited nodes\s*:\s*(\d+)", output)
                rows.append(dict(case=case, binary=label, command=command, code=code,
                                 verdict=verdict, seconds=seconds,
                                 nodes=int(nodes.group(1)) if nodes else None, output=output))
                samples[label].append(seconds)
                (HERE / ".local/comparison.json").write_text(json.dumps(rows, indent=2) + "\n")
                if label == "fixed":
                    want = "LIMIT" if expected is None else "UNSAFE" if expected else "SAFE"
                    assert verdict == want and code == (1 if expected is None else expected), rows[-1]
                else:
                    assert verdict != "ERROR", rows[-1]
        results = {label: sorted({row["verdict"] for row in rows
                                  if row["case"] == case and row["binary"] == label})
                   for label in binaries}
        print(f"{case}: {results}; median seconds "
              f"baseline={median(samples['baseline']):.4f}, fixed={median(samples['fixed']):.4f}",
              flush=True)
    print(f"Recorded {len(rows)} bounded executions in .local/comparison.json")


if __name__ == "__main__":
    main()
