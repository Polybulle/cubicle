#!/usr/bin/env python3
"""Bounded located-fixpoint acceptance checks; preserve real outputs in JSON."""
import argparse
import json
import os
from pathlib import Path
import re
import signal
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


def run(command, timeout=30):
    p = subprocess.Popen(list(map(str, command)), cwd=ROOT, text=True,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                         start_new_session=True)
    try:
        output, _ = p.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        output, _ = p.communicate()
        return dict(command=list(map(str, command)), code=p.returncode,
                    result="external-timeout", output=output)
    if p.returncode == 0 and "The system is SAFE" in output:
        result = "SAFE"
    elif p.returncode == 1 and "UNSAFE" in output:
        result = "UNSAFE"
    elif p.returncode == 1 and "Reached Limit !" in output:
        result = "limit"
    elif p.returncode == 0 and "PASS located fixpoint contracts" in output:
        result = "probe"
    else:
        result = "error"
    return dict(command=list(map(str, command)), code=p.returncode,
                result=result, output=output)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", type=Path, default=ROOT / "cubicle.opt")
    ap.add_argument("--probe", type=Path, default=HERE / ".local/check.opt")
    ap.add_argument("--cores", type=int, default=0)
    ap.add_argument("--solver", default="alt-ergo")
    ap.add_argument("--output", type=Path, default=HERE / ".local/results.json")
    args = ap.parse_args()
    records = []
    args.output.parent.mkdir(parents=True, exist_ok=True)

    def check(command, expected, timeout=30):
        record = run(command, timeout)
        records.append(record)
        args.output.write_text(json.dumps(records, indent=2) + "\n")
        assert record["result"] == expected, record
        return record

    common = ["-nocolor", "-nosubtyping", "-solver", args.solver, "-j", str(args.cores)]
    for mode in ("none", "fwd", "ignore", "bwd", "all"):
        record = check([args.probe, *common, "-tx", mode, HERE / "model.cub"], "probe", 120)
        if mode in ("bwd", "all"):
            assert re.findall(r"^node (\d+):", str(record["output"]), re.M) == ["1", "2"], record
    print("PASS direct contracts, finite oracle, and scheduler probe", flush=True)

    models = [
        (ROOT / "tests/neutral-candidates/internal-cycle.cub", "SAFE"),
        (ROOT / "tests/located-covering/swap-safe.cub", "SAFE"),
        (ROOT / "tests/located-covering/reachable-cycle-safe.cub", "SAFE"),
        (ROOT / "tests/located-covering/loop-exit-unsafe.cub", "UNSAFE"),
        (ROOT / "tests/neutral-candidates/internal-invariant-safe.cub", "SAFE"),
        (ROOT / "tests/neutral-candidates/internal-invariant-unsafe.cub", "UNSAFE"),
        (HERE / "nonconvergent.cub", "limit"),
    ]
    for model, expected in models:
        for search in ("bfs", "dfs"):
            for postpone in (0, 1, 2):
                for deletion in ([], ["-nodelete"]):
                    check([args.binary, *common, "-quiet", "-tx", "bwd", "-nodes", "40",
                           "-search", search, "-postpone", str(postpone), *deletion, model], expected)
        print(f"PASS {model.name}: BFS/DFS, postponement, deletion", flush=True)

    # Force the limit to be reached in the transaction, not at a neutral node.
    record = check([args.binary, *common, "-tx", "bwd", "-nodes", "2",
                    HERE / "nonconvergent.cub"], "limit")
    assert "spin" in str(record["output"]) and "node 3:" in str(record["output"]), record
    variants = [[], ["-nodelete"], ["-nosubtyping", "-nodelete"]]
    if args.solver == "z3":
        # Existing wrapper redeclares enumeration sorts during static subtyping.
        variants = [["-nosubtyping"], ["-nosubtyping", "-nodelete"]]
        print("Z3: static-subtyping runs excluded (existing sort redeclaration error)")
    for flags in variants:
        check([args.binary, "-nocolor", "-quiet", "-solver", args.solver, "-j", str(args.cores),
               "-nodes", "100", *flags, ROOT / "tests/gapped-witness/unsafe.cub"], "UNSAFE")
    print(f"PASS {len(records)} bounded executions (cores={args.cores}, solver={args.solver})")


if __name__ == "__main__":
    main()
