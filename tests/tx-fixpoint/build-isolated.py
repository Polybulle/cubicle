#!/usr/bin/env python3
"""Build an isolated source snapshot with an existing opam switch."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import signal

ROOT = Path(__file__).resolve().parents[2]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("destination", type=Path)
    ap.add_argument("--switch", required=True)
    ap.add_argument("--z3", action="store_true")
    args = ap.parse_args()
    dest = args.destination.resolve()
    dest.mkdir(parents=True, exist_ok=False)
    names = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=ROOT).decode().split("\0")
    names += ["configure"]
    for name in sorted(set(names) - {""}):
        src = ROOT / name
        if src.is_file():
            target = dest / name
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, target)
    prefix = ["/opt/homebrew/bin/opam", "exec", "--switch=" + args.switch, "--"]
    commands = [["./configure"] + (["--with-z3"] if args.z3 else []),
                ["make", "depend"], ["make"],
                ["make", "-f", "Makefile", "-f", "tests/tx-fixpoint/check.mk", "tx-fixpoint-check"]]
    if args.z3:
        zarith = subprocess.check_output(prefix + ["ocamlfind", "query", "zarith"], text=True).strip()
        libraries = "BIBOPT=nums.cmxa unix.cmxa functory.cmxa -I " + zarith + " zarith.cmxa z3ml.cmxa"
        commands[2].append(libraries)
        commands[3].append(libraries)
    with (dest / "build.log").open("w") as log:
        for cmd in commands:
            p = subprocess.Popen(prefix + cmd, cwd=dest, stdout=log, stderr=subprocess.STDOUT,
                                 start_new_session=True)
            try:
                code = p.wait(timeout=240)
            except subprocess.TimeoutExpired:
                os.killpg(p.pid, signal.SIGKILL)
                p.wait()
                raise
            if code:
                raise RuntimeError(f"{cmd} exited {code}; see {dest / 'build.log'}")
    print(dest, "build and probe compilation passed", flush=True)


if __name__ == "__main__":
    main()
