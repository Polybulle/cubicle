#!/usr/bin/env python3
"""CFG black-box checks. Nonzero means mismatches, never baseline waiver."""
import argparse
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import time
from typing import Any

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
EXPECTED = {
    'chain-safe': 'SAFE', 'chain-unsafe': 'UNSAFE',
    'internal-init-safe': 'SAFE', 'no-entry-safe': 'SAFE',
    'no-entry-zero-unsafe': 'UNSAFE', 'mixed-unsafe': 'UNSAFE',
    'yield-unsafe': 'UNSAFE', 'bindings-unsafe': 'UNSAFE',
    'control-only': 'UNSAFE', 'internal-cycle': 'SAFE',
    'ordinary-safe': 'SAFE', 'ordinary-unsafe': 'UNSAFE',
}

def classify(text, code):
    if code is None:
        return 'TIMEOUT'
    if 'Reached Limit' in text and code == 1:
        return 'LIMIT'
    if re.search(r'\bUNSAFE\s*!', text) and code == 1:
        return 'UNSAFE'
    if 'The system is SAFE' in text and code == 0:
        return 'SAFE'
    return 'ERROR'


def invoke(command, timeout=20, env=None) -> dict[str, Any]:
    start = time.monotonic()
    p = subprocess.Popen(command, cwd=REPO, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, text=True, start_new_session=True,
                         env=env)
    try:
        output, _ = p.communicate(timeout=timeout)
        code = p.returncode
    except subprocess.TimeoutExpired:
        os.killpg(p.pid, signal.SIGKILL)
        output, _ = p.communicate()
        code = None
    return dict(command=list(map(str, command)), code=code, output=output,
                seconds=time.monotonic()-start, outcome=classify(output, code))


def cases(matrix, parallel=False):
    for model, expected in EXPECTED.items():
        yield model, ['-tx', 'bwd'], expected
        if matrix:
            for strategy in ['bfs', 'bfsh', 'bfsa', 'dfs', 'dfsh', 'dfsa']:
                for postpone in ['0', '1', '2']:
                    yield model, ['-tx', 'bwd', '-search', strategy,
                                  '-postpone', postpone], expected
            yield model, ['-tx', 'bwd', '-nodelete'], expected
            if parallel:
                yield model, ['-tx', 'bwd', '-j', '2'], expected
    # Located covering closes this finite cycle even under small limits.
    # Actual nonconvergence/resource-limit checks live in tests/tx-fixpoint.
    yield 'internal-cycle', ['-tx', 'bwd', '-depth', '3', '-nodes', '300'], 'SAFE'
    yield 'internal-cycle', ['-tx', 'bwd', '-depth', '100000', '-nodes', '6'], 'SAFE'
    for model in ['ordinary-safe', 'ordinary-unsafe']:
        for mode in [[], ['-tx', 'none'], ['-tx', 'fwd'], ['-tx', 'ignore'], ['-tx']]:
            yield model, mode, EXPECTED[model]
    # Backward-only approximations: verdict check, not proof selection was reached.
    for model in ['chain-safe', 'chain-unsafe', 'internal-init-safe', 'ordinary-safe']:
        yield model, ['-tx', 'bwd', '-brab', '2'], EXPECTED[model]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, default=REPO/'cubicle.opt')
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--matrix', action='store_true')
    parser.add_argument('--parallel', action='store_true', help='include -j 2 in the matrix (requires real Functory)')
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    results = []
    for i, (model, options, expected) in enumerate(cases(args.matrix, args.parallel)):
        record = invoke([str(args.binary.resolve()), '-nocolor', '-depth', '12',
                         '-nodes', '300', *options, str(HERE/'models'/f'{model}.cub')])
        record.update(model=model, options=options, expected=expected,
                      passed=record['outcome'] == expected)
        (args.output/f'{i:03d}-{model}.log').write_text(record['output'])
        results.append(record)
    (args.output/'results.json').write_text(json.dumps(results, indent=2))
    print(json.dumps({'checks': len(results), 'passed': sum(r['passed'] for r in results),
                      'failures': [{k:r[k] for k in ['model', 'options', 'outcome', 'code', 'expected']}
                                   for r in results if not r['passed']]}, indent=2))
    return int(any(not r['passed'] for r in results))

if __name__ == '__main__':
    raise SystemExit(main())
