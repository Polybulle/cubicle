#!/usr/bin/env python3
"""Sequential functional checks for the commented transaction model presentations.

No build, warmup, timeout retry, benchmark ratio, or HIRR rerun is performed.
Reachability queries deliberately expect UNSAFE; they are not faulty protocols.
Each attempt retains full output, argv, exit, hashes, time, and search counters.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parent


def cases():
    rows = []

    def add(path, expected, mode="bwd", nodes=5000, extra=(), kind="safety"):
        rows.append(dict(path=path, expected=expected, mode=mode, nodes=nodes,
                         extra=list(extra), kind=kind))

    add("ironkv/ironkv-safe.cub", "SAFE")
    add("ironkv/ironkv-unsafe.cub", "UNSAFE", kind="mutant")
    add("ironkv/ironkv-completion-witness.cub", "UNSAFE", kind="reachability")
    add("german/german-bulk.cub", "SAFE", "none", 1500, ("-brab", "2"))
    add("german/german-incremental.cub", "SAFE", "none", 1500, ("-brab", "2"))
    add("german/german-incremental-tx.cub", "SAFE", "all", 1500, ("-brab", "2"))
    add("german/german-premature-grant-mutant.cub", "UNSAFE", "all", 10000,
        ("-brab", "2"), "mutant")
    for mode in ("bwd", "all"):
        add("jobhiring/jobhiring-safe.cub", "SAFE", mode)
        add("jobhiring/jobhiring-unsafe-notify.cub", "UNSAFE", mode, kind="mutant")
        add("jobhiring/jobhiring-notification-reachability.cub", "UNSAFE", mode,
            kind="reachability")
    add("2pc/two-phase-commit.cub", "SAFE", nodes=2000)
    add("2pc/two-phase-commit-missing-vote.cub", "UNSAFE", nodes=2000, kind="mutant")
    add("2pc/reach-two-rm-commit.cub", "UNSAFE", nodes=2000, kind="reachability")
    add("2pc/reach-prepared-abort.cub", "UNSAFE", nodes=2000, kind="reachability")
    return rows


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def classify(text, code, timed_out):
    if timed_out:
        return "TIMEOUT"
    if "Spurious trace" in text:
        return "SPURIOUS"
    if code == 0 and re.search(r"^The system is SAFE\s*$", text, re.M):
        return "SAFE"
    if code == 1 and re.search(r"^UNSAFE !\s*$", text, re.M):
        return "UNSAFE"
    return "ERROR_OR_LIMIT"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, default=REPO / "cubicle.opt")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--timeout", type=float, default=30)
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    binary = args.binary.resolve(strict=True)
    output = (args.output or ROOT / ".local" / time.strftime("run-%Y%m%d-%H%M%S")).resolve()
    output.mkdir(parents=True, exist_ok=False)
    suite = cases()
    binary_hash = digest(binary)
    print(f"Output: {output}", flush=True)
    records = []
    for index, case in enumerate(suite, 1):
        model = ROOT / case["path"]
        argv = [str(binary), "-j", "0", "-nocolor", "-tx", case["mode"],
                "-nodes", str(case["nodes"]), *case["extra"]]
        if case["expected"] == "UNSAFE":
            argv.append("-v")
        argv.append(str(model))
        log = output / f"{index:02d}-{model.stem}-{case['mode']}.log"
        started = time.monotonic()
        timed_out = False
        with log.open("w") as stream:
            process = subprocess.Popen(argv, cwd=REPO, stdout=stream,
                                       stderr=subprocess.STDOUT, start_new_session=True)
            try:
                code = process.wait(timeout=args.timeout)
            except subprocess.TimeoutExpired:
                timed_out = True
                os.killpg(process.pid, signal.SIGKILL)
                code = process.wait()
        elapsed = time.monotonic() - started
        text = log.read_text(errors="replace")
        outcome = classify(text, code, timed_out)
        counters = {key.strip(): int(value) for key, value in
                    re.findall(r"^([^\n:]+):\s*(\d+)\s*$", text, re.M)}
        record = dict(case, argv=argv, outcome=outcome, returncode=code,
                      passed=outcome == case["expected"], timeout_seconds=args.timeout,
                      wall_seconds=elapsed, log=str(log), counters=counters,
                      model_sha256=digest(model), binary_sha256=binary_hash)
        records.append(record)
        with (output / "results.jsonl").open("a") as stream:
            stream.write(json.dumps(record) + "\n")
        print(f"{index}/{len(suite)} {case['path']} [{case['mode']}]: {outcome}", flush=True)
    summary = dict(total=len(records), passed=sum(r["passed"] for r in records),
                   failed=sum(not r["passed"] for r in records),
                   scope="functional checks; no speedup claim", binary=str(binary),
                   binary_sha256=binary_hash)
    (output / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary), flush=True)
    return 0 if not summary["failed"] else 1


if __name__ == "__main__":
    sys.exit(main())
