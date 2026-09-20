#!/usr/bin/env python3
"""Boundary-only covering contracts while internal coverage is disabled."""
from pathlib import Path
import runpy

HERE = Path(__file__).resolve().parent
run = runpy.run_path(str(HERE.parent / "neutral-candidates/run.py"))["run"]

code, output = run([str(HERE / ".local/check.opt"), "-tx", "bwd", "-nodes", "100",
                    "-quiet", "-nocolor", str(HERE / "model.cub")])
assert code == 0 and "PASS boundary-only covering contracts" in output, output
print(output.strip())
for name, expected in (("swap-safe.cub", None), ("reachable-cycle-safe.cub", 0),
                       ("loop-exit-unsafe.cub", 1),
                       ("unbounded-internal.cub", None)):
    for postpone in (0, 1, 2):
        code, output = run(["./cubicle.opt", "-tx", "bwd", "-nodes", "20",
                            "-postpone", str(postpone), "-nocolor", "-quiet",
                            str(HERE / name)])
        if expected is None:
            assert code == 1 and "Reached Limit !" in output, output
            assert "UNSAFE" not in output and "The system is SAFE" not in output, output
        else:
            verdict = "UNSAFE" if expected else "The system is SAFE"
            assert code == expected and verdict in output, output
        print(f"PASS {name} postpone={postpone}")
