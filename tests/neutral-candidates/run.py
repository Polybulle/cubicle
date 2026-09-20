#!/usr/bin/env python3
"""Bound every verifier process group; never confuse limits with verdicts."""
import os
from pathlib import Path
import signal
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


def run(args, seconds=10):
    args = list(args) + ["-j", os.environ.get("TEST_CORES", "0")]
    proc = subprocess.Popen(args, cwd=ROOT, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True,
                            start_new_session=True)
    try:
        output, _ = proc.communicate(timeout=seconds)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        output, _ = proc.communicate()
        raise AssertionError(f"external timeout: {args}\n{output}")
    return proc.returncode, output


def main():
    model = str(HERE / "internal-invariant-unsafe.cub")
    for mode in ("bwd", "none"):
        command = [str(HERE / ".local/check.opt"), "-tx", mode,
                   "-nodes", "100", "-nocolor", "-quiet", model]
        code, output = run(command)
        assert code == 0 and "PASS neutral candidate contracts" in output, output
        print(f"PASS contracts -tx {mode}")
    cases = [("internal-invariant-unsafe.cub", 1),
             ("internal-invariant-safe.cub", 0),
             ("no-entry-safe.cub", 0), ("no-entry-unsafe.cub", 1),
             ("initial-invariant.cub", 0)]
    for name, expected in cases:
        for postpone in (0, 1, 2):
            command = ["./cubicle.opt", "-tx", "bwd", "-nodes", "100",
                       "-postpone", str(postpone), "-nocolor", "-quiet",
                       str(HERE / name)]
            code, output = run(command)
            verdict = "UNSAFE" if expected else "The system is SAFE"
            assert code == expected and verdict in output, (command, code, output)
            print(f"PASS {name} postpone={postpone}")
    code, output = run(["./cubicle.opt", "-tx", "bwd", "-nodes", "20",
                        "-nocolor", str(HERE / "internal-cycle.cub")])
    assert code == 1 and "Reached Limit !" in output, output
    assert "Number of visited nodes          : 21" in output, output
    assert "UNSAFE" not in output and "The system is SAFE" not in output, output
    print("PASS internal cycle reaches node limit with covering disabled")


if __name__ == "__main__":
    main()
