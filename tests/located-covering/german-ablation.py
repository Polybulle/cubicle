#!/usr/bin/env python3
"""Compare German with/without internal covering (sequential builds only)."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def run(binary, model, strategy, postpone, nodes):
    command = [str(binary), "-tx", "bwd", "-nodes", str(nodes), "-nocolor",
               "-search", strategy, "-postpone", str(postpone), "-j", "0", str(model)]
    proc = subprocess.Popen(command, cwd=ROOT, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, start_new_session=True)
    try:
        output, _ = proc.communicate(timeout=10)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        output, _ = proc.communicate()
        result = "external-timeout"
    else:
        if proc.returncode == 0 and "The system is SAFE" in output:
            result = "SAFE"
        elif proc.returncode == 1 and "UNSAFE" in output:
            result = "UNSAFE"
        elif "Reached Limit !" in output:
            result = "resource-limit"
        else:
            result = "error"
    return dict(command=command, code=proc.returncode, result=result, output=output)


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument("--enabled", required=True, type=Path)
    parser.add_argument("--disabled", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--nodes", type=int, default=100)
    args = parser.parse_args()
    model = ROOT / "examples/german_looped.cub"
    report = {
        "revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "model_sha256": hashlib.sha256(model.read_bytes()).hexdigest(),
        "ablation": "Sequential Bwd.Make: skip Fixpoint.check at internal nodes only; boundary checks, storage and deletion unchanged.",
        "timeout_seconds": 10,
        "cases": [],
    }
    for label in ("enabled", "disabled"):
        binary = getattr(args, label).resolve()
        report[label] = {"binary": str(binary),
                         "sha256": hashlib.sha256(binary.read_bytes()).hexdigest()}
        for strategy in ("bfs", "dfs"):
            for postpone in (0, 1, 2):
                record = run(binary, model, strategy, postpone, args.nodes)
                record.update(variant=label, search=strategy, postpone=postpone)
                match = re.search(r"Number of visited nodes\s*:\s*(\d+)", str(record["output"]))
                record["visited"] = int(match[1]) if match else None
                report["cases"].append(record)
                args.output.parent.mkdir(parents=True, exist_ok=True)
                args.output.write_text(json.dumps(report, indent=2) + "\n")
                print(label, strategy, postpone, record["result"], record["visited"], flush=True)
    assert len(report["cases"]) == 12


if __name__ == "__main__":
    main()
