#!/usr/bin/env python3
"""Regression verdicts plus independent finite exploration of the two models."""
from collections import deque
from itertools import permutations
import json
from pathlib import Path
import runpy

HERE = Path(__file__).resolve().parent
run = runpy.run_path(str(HERE.parent / "neutral-candidates/run.py"))["run"]
finite = runpy.run_path(str(HERE.parent / "forward-transactions/run.py"))


def control_reference(unsafe):
    tr = finite["tr"]
    return [
        tr("start", ("i", "j"), guard=[("X", 0)], updates=[("X", 1), ("A:i", 1)],
           calls=[("carry", ("j", "i"))], entry=True, yields=False),
        tr("carry", ("i", "j"), calls=[("finish", ("j" if unsafe else "i",))],
           yields=False),
        tr("finish", ("k",), guard=[("A:k", 1)], updates=[("Y", 1)])]


def reference(n, safe):
    initial: tuple[int, tuple[bool, ...], tuple[bool, ...]] = (
        1 if safe else 0, (False,) * n, (False,) * n)
    todo, seen = deque([initial]), {initial}
    while todo:
        phase, a, b = todo.popleft()
        if any((phase == 1 and a[x] and a[y] and not safe) or
               (phase == 2 and b[x] and a[y])
               for x, y in permutations(range(n), 2)):
            return True
        for i in range(n):
            after = None
            if phase == 0 and not safe:
                after = (1, tuple(j == i for j in range(n)), b)
            elif phase == 1:
                after = (2, a, tuple(j == i for j in range(n)))
            if after is not None and after not in seen:
                seen.add(after)
                todo.append(after)
    return False


def main():
    results = []
    for n in range(1, 6):
        assert reference(n, False) == (n >= 2)
        assert not reference(n, True)
    print("PASS independent finite exploration, populations 1 through 5")
    for unsafe in (False, True):
        spec = control_reference(unsafe)
        name = "control-unsafe.cub" if unsafe else "control-safe.cub"
        assert (HERE / name).read_text() == finite["model"](spec)
        for n in (1, 2, 3):
            states = finite["reference"](spec, n, True, -1)
            assert any(mask & 2 for mask in states) == (unsafe and n >= 2)
    print("PASS independent control-binding exploration, populations 1 through 3")
    for mode in ("none", "bwd"):
        for model in ("model.cub", "control-safe.cub", "control-unsafe.cub"):
            command = [str(HERE / ".local/check.opt"), "-max-procs", "16",
                       "-tx", mode, "-quiet", "-nocolor",
                       str(HERE / model)]
            code, output = run(command)
            assert code == 0 and "PASS normalized variable contracts" in output, output
            print(f"{output.strip()} -tx {mode} {model}")
    for name, expected in (("unsafe.cub", 1), ("safe.cub", 0),
                           ("control-unsafe.cub", 1), ("control-safe.cub", 0)):
        for mode in ("none", "bwd", "all"):
            expected_mode = 1 if name == "control-safe.cub" and mode == "none" else expected
            for search in ("bfs", "bfsh", "bfsa", "dfs", "dfsh", "dfsa"):
                for extra in ([], ["-nosubtyping", "-nodelete"]):
                    command = ["./cubicle.opt", "-quiet", "-nocolor", "-nodes", "100",
                               "-tx", mode, "-search", search, *extra, str(HERE / name)]
                    code, output = run(command)
                    results.append(dict(command=command, code=code, output=output))
                    (HERE / ".local/results.json").write_text(json.dumps(results, indent=2) + "\n")
                    verdict = "UNSAFE" if expected_mode else "The system is SAFE"
                    assert code == expected_mode and verdict in output, results[-1]
        print(f"PASS {name}: transaction modes, search strategies, subtyping/deletion toggles")
    print(f"PASS {len(results)} end-to-end cases")


if __name__ == "__main__":
    main()
