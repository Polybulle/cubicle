#!/usr/bin/env python3
"""Run structural contracts in isolated processes (Options is initialized once)."""
import argparse
import json
import os
from pathlib import Path
from run import HERE, REPO, invoke

CASES = {'cfg-state':'control-only', 'neutral-transfer':'bindings-unsafe',
         'mixed':'mixed-unsafe', 'trace':'chain-unsafe',
         'boundary-first':'chain-safe', 'internal-first':'chain-safe',
         'internal-initial':'internal-init-safe'}


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--binary', type=Path, default=HERE/'.local/probe.opt')
    p.add_argument('--parallel', action='store_true', help='also test -j 2 (requires real Functory)')
    a = p.parse_args()
    a.output.mkdir(parents=True, exist_ok=False)
    results = []
    for case, model in CASES.items():
        variants = [[], ['-nodelete']] if case.startswith(('boundary', 'internal')) else [[]]
        if a.parallel and case.startswith(('boundary', 'internal')):
            variants.append(['-j', '2'])
        for flags in variants:
            visits = a.output.resolve()/f'visits-{len(results):02d}.txt'
            visits.touch(exist_ok=False)
            env = dict(os.environ, ISOQA_CASE=case, ISOQA_VISITS=str(visits))
            r = invoke([str(a.binary.resolve()), '-nocolor', '-tx', 'bwd',
                        '-depth', '12', '-nodes', '300', *flags,
                        str(HERE/'models'/f'{model}.cub')], env=env)
            r.update(case=case, flags=flags,
                     passed=r['code']==0 and f'PASS {case}' in r['output'])
            results.append(r)
    (a.output/'results.json').write_text(json.dumps(results, indent=2))
    for r in results:
        print(r['case'], r['flags'], 'PASS' if r['passed'] else 'FAIL',
              r['output'].strip().splitlines()[-1])
    return int(any(not r['passed'] for r in results))

if __name__ == '__main__':
    raise SystemExit(main())
