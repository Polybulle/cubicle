#!/usr/bin/env python3
"""Compare internal-covering enabled/disabled binaries under identical bounds."""
import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
MODELS = [
    HERE.parent / "neutral-candidates/internal-cycle.cub",
    HERE / "swap-safe.cub",
    HERE / "reachable-cycle-safe.cub",
    HERE / "loop-exit-unsafe.cub",
    HERE / "unbounded-internal.cub",
    HERE.parent / "neutral-candidates/internal-invariant-safe.cub",
    HERE.parent / "neutral-candidates/internal-invariant-unsafe.cub",
]


def run(binary, model, strategy, postpone, cores):
    command = [str(binary), "-tx", "bwd", "-nodes", "100", "-nocolor", "-quiet",
               "-search", strategy, "-postpone", str(postpone), "-j", str(cores), str(model)]
    proc = subprocess.Popen(command, cwd=ROOT, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, start_new_session=True)
    try:
        out, _ = proc.communicate(timeout=10)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        out, _ = proc.communicate()
        result = "external-timeout"
    else:
        if proc.returncode == 0 and "The system is SAFE" in out:
            result = "SAFE"
        elif proc.returncode == 1 and "UNSAFE" in out:
            result = "UNSAFE"
        elif proc.returncode == 1 and "Reached Limit !" in out:
            result = "node-limit"
        else:
            raise AssertionError((command, proc.returncode, out))
    return dict(command=command, code=proc.returncode, result=result, output=out)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--before", required=True, type=Path)
    parser.add_argument("--after", type=Path, default=ROOT / "cubicle.opt")
    parser.add_argument("--cores", type=int, default=0)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    before, after = args.before.resolve(), args.after.resolve()
    cases = []
    report = dict(before=str(before), after=str(after), cores=args.cores,
                  before_sha256=hashlib.sha256(before.read_bytes()).hexdigest(),
                  after_sha256=hashlib.sha256(after.read_bytes()).hexdigest(), cases=cases)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    for model in MODELS:
        changes = Counter()
        for strategy in ("bfs", "dfs"):
            for postpone in (0, 1, 2):
                old = run(before, model, strategy, postpone, args.cores)
                new = run(after, model, strategy, postpone, args.cores)
                cases.append(dict(model=model.name, strategy=strategy,
                                  postpone=postpone, before=old, after=new))
                args.output.write_text(json.dumps(report, indent=2) + "\n")
                changes[(old["result"], new["result"])] += 1
        print(model.name, dict(changes), flush=True)
    keys = {(c["model"], c["strategy"], c["postpone"]) for c in cases}
    assert len(keys) == len(cases) == len(MODELS) * 6
    print("Compared", len(cases), "bounded before/after pairs")


if __name__ == "__main__":
    main()
