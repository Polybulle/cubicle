#!/usr/bin/env python3
"""Differential finite-state tests against an independent labelled interpreter."""
from collections import deque
from itertools import permutations
import json
import os
from pathlib import Path
import signal
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
LOCAL = HERE / ".local"


def tr(name, args=(), guard=(), updates=(), calls=(), entry=False, yields=True):
    return dict(name=name, args=args, guard=guard, updates=updates,
                calls=calls, entry=entry, yields=yields)


SPECS = {
    "no-interleaving": [
        tr("start", guard=[("X", 0)], updates=[("X", 1)],
           calls=[("finish", ())], entry=True, yields=False),
        tr("finish", guard=[("X", 1)], updates=[("X", 0)]),
        tr("intrude", guard=[("X", 1)], updates=[("Y", 1)], entry=True)],
    "same-data-new-location": [
        tr("start", calls=[("middle", ())], entry=True, yields=False),
        tr("middle", calls=[("finish", ())], yields=False),
        tr("finish", updates=[("Y", 1)])],
    "swapped-cycle": [
        tr("start", ("i", "j"), updates=[("A:i", 1)],
           calls=[("spin", ("i", "j"))], entry=True, yields=False),
        tr("spin", ("i", "j"), guard=[("A:i", 1)], updates=[("A:j", 1)],
           calls=[("spin", ("j", "i")), ("finish", ("j",))], yields=False),
        tr("finish", ("i",), guard=[("A:i", 1)], updates=[("Y", 1)])],
    "underscore-distinct": [
        tr("start", ("i", "j"), guard=[("X", 0)], updates=[("X", 1), ("A:i", 1)],
           calls=[("finish", ("i", None))], entry=True, yields=False),
        tr("finish", ("i", "j"), guard=[("A:j", 1)], updates=[("Y", 1)])],
    "underscore-reuse": [
        tr("start", ("i",), guard=[("X", 0)], updates=[("X", 1), ("A:i", 1)],
           calls=[("finish", (None,))], entry=True, yields=False),
        tr("finish", ("j",), guard=[("A:j", 1)], updates=[("Y", 1)])],
    "no-entry": [tr("finish", updates=[("Y", 1)])],
    "yield-or-continue": [
        tr("start", guard=[("X", 0)], updates=[("X", 1)],
           calls=[("spin", ())], entry=True),
        tr("spin", updates=[("X", 0), ("Y", 1)], calls=[("spin", ())]),
        tr("other", guard=[("X", 1)], updates=[("Y", 1)], entry=True)],
    "depth-join": [
        tr("start", calls=[("long", ()), ("short", ())], entry=True, yields=False),
        tr("long", calls=[("extra", ())], yields=False),
        tr("extra", calls=[("finish", ())], yields=False),
        tr("short", calls=[("finish", ())], yields=False),
        tr("finish", updates=[("Y", 1)])],
}


def term(text):
    return "A[" + text[2:] + "]" if text.startswith("A:") else text


def model(spec):
    lines = ["type bit = Zero | One", "var X : bit", "var Y : bit",
             "array A[proc] : bit", "init (i) { X = Zero && Y = Zero && A[i] = Zero }",
             "unsafe { Y = One }"]
    for t in spec:
        guard = " && ".join(f"{term(x)} = {'One' if v else 'Zero'}" for x, v in t["guard"])
        updates = " ".join(f"{term(x)} := {'One' if v else 'Zero'};" for x, v in t["updates"])
        prefix = "" if t["entry"] else "triggered "
        lines.append(f"{prefix}transition {t['name']} ({' '.join(t['args'])}) "
                     f"requires {{ {guard or 'X = X'} }} {{ {updates or 'X := X;'} }}")
        if t["calls"]:
            calls = [f"{name} ({' '.join(a if a is not None else '_' for a in args)})"
                     for name, args in t["calls"]]
            if t["yields"]:
                calls.append("yields")
            lines.append("triggers " + " or ".join(calls))
    return "\n".join(lines) + "\n"


def reference(spec, n, located, depth_limit, max_forward=-1):
    by_name = {t["name"]: t for t in spec}
    initial = (0,) * (n + 2)
    queue: deque[tuple[int, tuple[int, ...], str | None, tuple[int, ...]]] = deque(
        [(0, initial, None, ())])
    seen = {}
    reached = set()
    count = 0

    def index(x, bindings):
        return 2 + bindings[x[2:]] if x.startswith("A:") else (0 if x == "X" else 1)

    while queue and (max_forward == -1 or count < max_forward):
        depth, data, location, args = queue.popleft()
        key = (data, location, args) if located else data
        if key in seen and seen[key] <= depth:
            continue
        seen[key] = depth
        count += 1
        reached.add(sum(v << i for i, v in enumerate(data)))
        if located and location is None:
            for t in spec:
                if t["entry"]:
                    for actual in permutations(range(n), len(t["args"])):
                        queue.append((depth, data, t["name"], actual))
            continue
        if depth_limit >= 0 and depth >= depth_limit:
            continue
        todo = [(by_name[location], args)] if located else [
            (t, actual) for t in spec for actual in permutations(range(n), len(t["args"]))]
        for t, actual in todo:
            bindings = dict(zip(t["args"], actual))
            if not all(data[index(x, bindings)] == v for x, v in t["guard"]):
                continue
            after = list(data)
            for x, v in t["updates"]:
                after[index(x, bindings)] = v
            after = tuple(after)
            if not located or t["yields"]:
                queue.append((depth + 1, after, None, ()))
            if located:
                for callee, passed in t["calls"]:
                    # Enumerate whole injective tuples, then enforce named equalities.
                    for chosen in permutations(range(n), len(by_name[callee]["args"])):
                        if all(p is None or bindings[p] == q for p, q in zip(passed, chosen)):
                            queue.append((depth + 1, after, callee, chosen))
    return reached


def execute(command, env, seconds=10):
    p = subprocess.Popen(command, cwd=ROOT, env=env, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, text=True, start_new_session=True)
    try:
        out, _ = p.communicate(timeout=seconds)
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        out, _ = p.communicate()
        raise AssertionError(f"external timeout: {command}\n{out}")
    return p.returncode, out


def run(command, env):
    code, out = execute(command, env)
    assert code == 0 and "PASS forward probe" in out, (command, code, out)
    return {kind: {int(line.split()[1]) for line in out.splitlines() if line.startswith(kind + " ")}
            for kind in ("SYMBOLIC", "ENUMERATIVE")}


def main():
    LOCAL.mkdir(exist_ok=True)
    results = []
    for name, spec in SPECS.items():
        path = LOCAL / (name + ".cub")
        path.write_text(model(spec))
        cases = [(n, mode, depth, -1, False) for n in (1, 2, 3)
                 for mode in ("all", "fwd") for depth in (-1, 0, 1, 2, 3)]
        cases += [(2, mode, -1, -1, False) for mode in ("none", "bwd")]
        cases += [(2, "all", -1, limit, False) for limit in (0, 1)]
        cases += [(3, "all", -1, -1, True)]
        for n, mode, depth, limit, nosym in cases:
            command = [str(LOCAL / "check.opt"), "-tx", mode, "-quiet", "-nocolor",
                       "-nodes", "100", "-max-forward", str(limit),
                       "-forward-depth", str(depth), str(path)]
            if nosym:
                command.append("-forward-nosym")
            actual = run(command, dict(os.environ, TEST_PROCS=str(n)))
            expected = reference(spec, n, mode in ("all", "fwd"), depth, limit)
            case = dict(model=name, n=n, mode=mode, depth=depth, limit=limit, nosym=nosym,
                        expected=sorted(expected), actual={k: sorted(v) for k, v in actual.items()})
            results.append(case)
            (LOCAL / "results.json").write_text(json.dumps(results, indent=2) + "\n")
            assert all(states == expected for states in actual.values()), case
        print(f"PASS {name}: {len(cases)} differential cases", flush=True)
    print(f"PASS {len(results)} differential cases; both engines match the finite interpreter")
    checks = [(LOCAL / "no-interleaving.cub", "all", 0),
              (LOCAL / "no-interleaving.cub", "fwd", 1),
              (LOCAL / "swapped-cycle.cub", "all", 1),
              (LOCAL / "no-entry.cub", "all", 0),
              (HERE.parent / "located-covering/loop-exit-unsafe.cub", "all", 1),
              (ROOT / "examples/german.cub", "all", 0)]
    end_to_end = []
    for path, mode, expected in checks:
        command = ["./cubicle.opt", "-tx", mode, "-brab", "2", "-nodes", "5000",
                   "-max-forward", "100000", "-quiet", "-nocolor",
                   "-j", os.environ.get("TEST_CORES", "0"), str(path)]
        code, out = execute(command, os.environ, seconds=30)
        verdict = "UNSAFE" if expected else "The system is SAFE"
        assert code == expected and verdict in out, (command, code, out)
        end_to_end.append(dict(command=command, code=code, output=out))
        (LOCAL / "end-to-end.json").write_text(json.dumps(end_to_end, indent=2) + "\n")
        print(f"PASS end-to-end {mode} {path.name}", flush=True)


if __name__ == "__main__":
    main()
