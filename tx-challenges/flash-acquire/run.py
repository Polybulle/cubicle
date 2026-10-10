#!/usr/bin/env python3
"""Sequential FLASH benchmarks; run from any directory with Python 3.

Default: strict/eager x fwd,bwd,all,ignore; four bwd witness controls;
and examples/flash.cub with its historical ignore/BRAB-3/depth-13 recipe.
Logs and append-only results.jsonl live beside this script under .local/.
--select accepts a manifest filename; --mode filters (never overrides) recipes.
No builds, warmups, typechecks, retries, or parallel checker executions.
"""

import argparse
import datetime
import hashlib
import json
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import threading
import time
import uuid

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
BINARY = REPO / "cubicle.opt"
MODES = ("fwd", "bwd", "all", "ignore")
CONTROLS = (
    "flash-premature-grant.cub",
    "flash-stale-owner.cub",
    "flash-ignored-invalidation.cub",
    "flash-completion-witness.cub",
)
COUNTERS = {
    "visited_nodes": r"Number of visited nodes\s*:\s*(\d+)",
    "forward_nodes": r"Total forward nodes\s*:\s*(\d+)",
    "solver_calls": r"Number of solver calls\s*:\s*(\d+)",
    "invariants": r"Number of invariants\s*:\s*(\d+)",
    "restarts": r"Restarts\s*:\s*(\d+)",
    "max_processes": r"Max Number of processes\s*:\s*(\d+)",
}
ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")


def manifest():
    jobs = []
    for name in ("flash-acquire-strict.cub", "flash-acquire-eager.cub"):
        for mode in MODES:
            flags = ["-tx", mode]
            if mode != "bwd":
                flags += ["-brab", "2"]
            jobs.append((HERE / name, mode, flags))
    for name in CONTROLS:
        jobs.append((HERE / name, "bwd", ["-tx", "bwd", "-v"]))
    jobs.append((REPO / "examples" / "flash.cub", "ignore",
                 ["-tx", "ignore", "-brab", "3", "-forward-depth", "13"]))
    return jobs


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def parse_stats(text):
    text = ANSI.sub("", text)
    stats = {}
    for key, pattern in COUNTERS.items():
        matches = re.findall(pattern, text, flags=re.IGNORECASE)
        # Stats may appear after restarts or an interrupt: retain the last report.
        stats[key] = int(matches[-1]) if matches else None
    return stats


def classify(text, exit_code, timed_out=False, interrupted=False, launch_error=None):
    text = ANSI.sub("", text)
    if timed_out:
        return "timeout"
    if interrupted:
        return "interrupted"
    if launch_error:
        return "error"
    if re.search(r"\bReached Limit\b", text, re.IGNORECASE):
        return "limit"
    if re.search(r"\bSpurious trace\b", text, re.IGNORECASE):
        return "spurious"
    if re.search(r"(?:lexical|syntax|typing|solver|fatal) error|internal failure|ABORT !",
                 text, re.IGNORECASE):
        return "error"
    safe = bool(re.search(r"^\s*The system is SAFE\s*$", text, re.MULTILINE))
    unsafe = bool(re.search(r"^\s*UNSAFE\s*!\s*$", text, re.MULTILINE))
    if safe and not unsafe and exit_code == 0:
        return "SAFE"
    if unsafe and not safe and exit_code == 1:
        return "UNSAFE"
    if exit_code is not None and exit_code < 0:
        return "signal"
    return "error"


def kill_group(process, sig):
    try:
        os.killpg(process.pid, sig)
    except ProcessLookupError:
        pass


def execute(argv, log_path, timeout):
    """Blocking wait; an external watchdog terminates the entire new session."""
    timed_out = threading.Event()
    interrupted = False
    launch_error = None
    exit_code = None
    process = None
    watchdog = None
    escalation = None
    with log_path.open("xb") as log:
        start = time.monotonic()
        try:
            process = subprocess.Popen(argv, cwd=REPO, stdin=subprocess.DEVNULL,
                                       stdout=log, stderr=subprocess.STDOUT,
                                       start_new_session=True)

            def expire():
                nonlocal escalation
                timed_out.set()
                kill_group(process, signal.SIGTERM)
                escalation = threading.Timer(2.0, kill_group,
                                             args=(process, signal.SIGKILL))
                escalation.daemon = True
                escalation.start()

            watchdog = threading.Timer(timeout, expire)
            watchdog.daemon = True
            watchdog.start()
            try:
                exit_code = process.wait()
            except KeyboardInterrupt:
                interrupted = True
                kill_group(process, signal.SIGKILL)
                exit_code = process.wait()
        except OSError as exc:
            launch_error = str(exc)
            log.write(("Runner launch error: " + launch_error + "\n").encode())
        finally:
            if watchdog is not None:
                watchdog.cancel()
                watchdog.join()
            if escalation is not None:
                escalation.cancel()
                escalation.join()
            if process is not None and (timed_out.is_set() or interrupted):
                # The leader may exit on TERM before its descendants do.
                kill_group(process, signal.SIGKILL)
            wall_seconds = time.monotonic() - start
    text = log_path.read_text(encoding="utf-8", errors="replace")
    return dict(exit_code=exit_code, wall_seconds=wall_seconds,
                timed_out=timed_out.is_set(), interrupted=interrupted,
                launch_error=launch_error,
                outcome=classify(text, exit_code, timed_out.is_set(),
                                 interrupted, launch_error), **parse_stats(text))


def positive_timeout(value):
    number = float(value)
    if not math.isfinite(number) or number <= 0:
        raise argparse.ArgumentTypeError("timeout must be finite and positive")
    return number


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--select", metavar="FILENAME",
                        help="run only this manifest filename (e.g. flash-strict.cub)")
    parser.add_argument("--mode", choices=MODES, help="filter transaction mode")
    parser.add_argument('--brab', type=int, choices=(2,3), default=None,
                        help='explicit supplemental finite-instance size; replaces recipe brab setting')
    parser.add_argument('--search', choices=('bfs','dfs'), default=None,
                        help='optional explicit search strategy for supplemental controls')
    parser.add_argument("--timeout", type=positive_timeout, default=450.0,
                        help="external deadline per execution in seconds (default: 450)")
    args = parser.parse_args(argv)
    jobs = manifest()
    names = {path.name for path, _, _ in jobs}
    if args.select is not None and args.select not in names:
        parser.error("unknown filename; choose from: " + ", ".join(sorted(names)))
    jobs = [(path, mode, flags) for path, mode, flags in jobs
            if (args.select is None or path.name == args.select)
            and (args.mode is None or mode == args.mode)]
    if not jobs:
        parser.error("selection has no matching benchmark recipes")
    # Validate the full selection before creating outputs or running any checker.
    if not BINARY.is_file() or not os.access(BINARY, os.X_OK):
        parser.error("native executable missing or not executable: " + str(BINARY))
    for path, _, _ in jobs:
        if not path.is_file():
            parser.error("model missing: " + str(path))
    binary_hash = sha256(BINARY)
    input_hashes = {path: sha256(path) for path, _, _ in jobs}
    local = HERE / ".local"
    local.mkdir(exist_ok=True)
    snapshots = local / 'inputs'
    snapshots.mkdir(exist_ok=True)
    for path, digest in input_hashes.items():
        target = snapshots / (digest + '.cub')
        if not target.exists():
            target.write_bytes(path.read_bytes())
    run_id = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%S")
    run_id += "-" + uuid.uuid4().hex
    failed = False
    for index, (path, mode, flags) in enumerate(jobs, 1):
        if args.brab is not None:
            flags = list(flags)
            if '-brab' in flags:
                index_brab = flags.index('-brab')
                del flags[index_brab:index_brab+2]
            flags += ['-brab', str(args.brab)]
        log_path = local / (f"{run_id}-{index:02d}-{path.stem}-{mode}.log")
        command = [str(BINARY), "-j", "0", "-nocolor", *flags,
                   *(['-search', args.search] if args.search else []), str(path)]
        # Refuse stale provenance if a model or executable changes during the batch.
        if sha256(BINARY) != binary_hash or sha256(path) != input_hashes[path]:
            print("Input or binary changed during the batch; stopping.", file=sys.stderr)
            return 2
        print(f"[{index}/{len(jobs)}] {path.name} {mode}", flush=True)
        result = execute(command, log_path, args.timeout)
        record = dict(run_id=run_id, model=path.name, input_path=str(path), mode=mode,
                      argv=command, cwd=str(REPO), timeout_seconds=args.timeout,
                      input_sha256=input_hashes[path], binary_sha256=binary_hash,
                      log_path=str(log_path),
                      recorded_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                      **result)
        with (local / "results.jsonl").open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record, sort_keys=True, allow_nan=False) + "\n")
            stream.flush()
            os.fsync(stream.fileno())
        print(f"  {result['outcome']} exit={result['exit_code']} "
              f"wall={result['wall_seconds']:.3f}s", flush=True)
        failed |= result["outcome"] not in ("SAFE", "UNSAFE")
        if result["interrupted"]:
            return 130
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
