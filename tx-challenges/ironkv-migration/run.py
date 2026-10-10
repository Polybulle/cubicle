#!/usr/bin/env python3
"""Sequential bounded checks; no build, weakening or fabricated verdicts.

Retry limits with BRAB2; M1/M3 retain their successful BRAB3 recipes.
M4/M5 use baseline, BRAB3 BFS, then BRAB3 BFSh after a resource limit.
No SAFE claim is made when limits are exhausted. Read results.jsonl, not the
runner exit status, for verification outcomes.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import time

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, default=REPO / 'cubicle.opt')
    parser.add_argument('--timeout', type=int, default=120)
    parser.add_argument('--nodes', type=int, default=1500)
    parser.add_argument('--brab', type=int, help='single extra approximation recipe; default retries with 2')
    parser.add_argument('--only', nargs='*')
    parser.add_argument('--modes', nargs='*', choices=['bwd', 'all', 'ignore'])
    parser.add_argument('--search', choices=['bfs', 'dfs', 'bfsh'], default='bfs')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    binary = args.binary.resolve(strict=True)
    timeout = shutil.which('timeout') or shutil.which('gtimeout')
    if not timeout:
        parser.error('GNU timeout or gtimeout is required')
    output = args.output or HERE / '.local' / time.strftime('run-%Y%m%d-%H%M%S')
    output.mkdir(parents=True, exist_ok=False)
    cases = [('ironkv-migration.cub', mode) for mode in ('bwd', 'all', 'ignore')]
    cases += [(name + '.cub', 'bwd') for name in ('m1-no-dedup', 'm2-reset-watermark',
                                               'm3-retain-inbox', 'm4-sequence-reuse',
                                               'm5-wrong-endpoint-ack', 'completion-witness')]
    if args.only:
        cases = [(f, m) for f, m in cases if f in args.only]
    if args.modes:
        cases = [(f, m) for f, m in cases if m in args.modes]
    results = []
    for filename, mode in cases:
        recipes = [(0, args.search), (2, args.search)] if args.brab is None else [(args.brab, args.search)]
        if args.brab is None and filename == 'm1-no-dedup.cub':
            recipes.append((3, 'bfs'))
        if args.brab is None and filename == 'm3-retain-inbox.cub':
            recipes.append((3, 'bfsh'))
        if args.brab is None and filename in ('m4-sequence-reuse.cub', 'm5-wrong-endpoint-ack.cub'):
            recipes = [(0, args.search), (3, 'bfs'), (3, 'bfsh')]
        for brab, search in recipes:
            model = HERE / filename
            label = f'{model.stem}-{mode}-{search}' + ('-brab' + str(brab) if brab else '')
            argv = [timeout, str(args.timeout), str(binary), '-j', '0', '-nocolor',
                    '-tx', mode, '-nodes', str(args.nodes), '-out', str(output),
                    '-search', search]
            if brab:
                argv += ['-brab', str(brab)]
            argv += ['-v', str(model)]
            log = output / (label + '.log')
            start = time.monotonic()
            with log.open('w') as stream:
                stream.write('ARGV ' + json.dumps(argv) + '\n')
                stream.flush()
                result = subprocess.run(argv, cwd=HERE, stdout=stream, stderr=subprocess.STDOUT)
            seconds = time.monotonic() - start
            text = log.read_text(errors='replace')
            code = result.returncode
            if code == 124:
                verdict = 'TIMEOUT'
            elif 'Spurious trace' in text:
                verdict = 'SPURIOUS'
            elif code == 0 and re.search(r'^The system is SAFE\s*$', text, re.M):
                verdict = 'SAFE'
            elif code == 1 and re.search(r'^UNSAFE !\s*$', text, re.M):
                verdict = 'UNSAFE'
            elif 'Reached Limit !' in text or re.search(r'(maximum|maximal|limit|Maximum).*nodes|nodes.*(maximum|limit)', text, re.I):
                verdict = 'NODE_LIMIT'
            else:
                verdict = 'ERROR_OR_LIMIT'
            counters = {k.strip(): int(v) for k, v in
                        re.findall(r'^([^\n:]+):\s*(\d+)', text, re.M)}
            record = dict(file=filename, mode=mode, search=search, brab=brab if brab else None,
                          argv=argv, verdict=verdict, exit_code=code, seconds=seconds,
                          node_limit=args.nodes, timeout_seconds=args.timeout,
                          counters=counters, log=str(log), model_sha256=digest(model),
                          binary_sha256=digest(binary))
            results.append(record)
            with (output / 'results.jsonl').open('a') as stream:
                stream.write(json.dumps(record) + '\n')
            print(f'{label}: {verdict} exit={code} seconds={seconds:.3f}', flush=True)
            if verdict not in ('TIMEOUT', 'NODE_LIMIT', 'ERROR_OR_LIMIT'):
                break
            if verdict == 'ERROR_OR_LIMIT' and ('Syntax error' in text or 'Typing error' in text):
                break
    print(f'Results: {output / "results.jsonl"}', flush=True)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
