#!/usr/bin/env python3
"""Curated Cubicle comparison. Uses immutable git archives, never checkout/reset.

Default is a read-only plan. Build/pilot/run require a new output directory.
Timing uses perf_counter around direct subprocesses, not a Python timeout wrapper
inside hyperfine. Every measured run retains its verdict and complete output.
"""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import random
import re
import signal
import statistics
import subprocess
import sys
import threading
import time

ROOT = Path(__file__).resolve().parents[1]
CONFIGS = {
    'baseline': ('baseline', []),
    'tetra-none': ('tetra', ['-tx', 'none']),
    'tetra-all': ('tetra', ['-tx', 'all']),
    'old-none': ('old', ['-tx', 'none']),
    'old-all': ('old', ['-tx', 'all']),
}
COMMON = ['-quiet', '-nocolor', '-solver', 'alt-ergo']


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def git(*args):
    return subprocess.check_output(['git', '-C', str(ROOT), *args])


def configurations(model):
    return ['tetra-all', 'old-all'] if model['group'] == 'transaction' else list(CONFIGS)


def classify(text, code, timed_out=False, type_only=False):
    if timed_out:
        return 'timeout', None
    if code is not None and code < 0:
        return 'crash', None
    # UNSAFE may use nonzero exit status; a counterexample is not a command error.
    verdicts = set(re.findall(r'The system is (SAFE|UNSAFE)\b', text))
    if re.search(r'^UNSAFE\b', text, re.MULTILINE):
        verdicts.add('UNSAFE')
    if len(verdicts) == 1:
        verdict = next(iter(verdicts))
        if verdict == 'SAFE' and code != 0:
            return 'error', None
        return 'completed', verdict
    if len(verdicts) > 1:
        return 'conflicting-output', None
    if type_only and code == 0:
        return 'typechecked', None
    if re.search(r'cycle.*(?:forbidden|trigger)|(?:forbidden|trigger).*cycle|requires -tx', text, re.I):
        return 'unsupported', None
    if re.search(r'(?:syntax|parse|typing|type) error', text, re.I):
        return 'input-error', None
    if re.search(r'limit|maximum|too many|unknown', text, re.I):
        return 'limit-or-unknown', None
    return ('error' if code != 0 else 'no-verdict'), None


def invoke(command, cwd, timeout, log, type_only=False):
    start = time.perf_counter()
    timed_out = False
    with log.open('wb') as stream:
        proc = subprocess.Popen(command, cwd=cwd, stdout=stream,
                                stderr=subprocess.STDOUT, start_new_session=True)

        def expire():
            nonlocal timed_out
            if proc.poll() is None:
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                    timed_out = True
                except ProcessLookupError:
                    pass

        timer = threading.Timer(timeout, expire)
        timer.start()
        try:
            code = proc.wait()
        except BaseException:
            if proc.poll() is None:
                os.killpg(proc.pid, signal.SIGKILL)
            proc.wait()
            raise
        finally:
            timer.cancel()
            timer.join()
    elapsed = time.perf_counter() - start
    text = log.read_text(errors='replace')
    status, verdict = classify(text, code, timed_out, type_only)
    stats = dict(re.findall(r'^\s*([^\n:]+?)\s*:\s*(\d+)\s*$', text, re.MULTILINE))
    return {'command': command, 'status': status, 'verdict': verdict,
            'returncode': code, 'wall_seconds': elapsed, 'timeout_seconds': timeout,
            'log': str(log), 'stats': stats}


def select(manifest, names):
    if not names:
        return manifest['models']
    wanted = set(names)
    models = [m for m in manifest['models'] if m['path'] in wanted or Path(m['path']).name in wanted]
    matched = {name for name in wanted if any(name in (m['path'], Path(m['path']).name) for m in models)}
    if matched != wanted:
        raise ValueError('Unknown/unselected models: ' + ', '.join(sorted(wanted - matched)))
    return models


def make_command(num_directory, revision):
    return ['make',
            'INCLUDES=$(INCLPATHS) $(Z3CCFLAGS) -I ' + num_directory + ' -I +unix',
            'VERSION_STR=' + revision]


def build(manifest, out):
    dirs = {}
    num_directory = subprocess.check_output(['ocamlfind', 'query', 'num']).decode().strip()
    for name, spec in manifest['builds'].items():
        if git('rev-parse', spec['commit'] + '^{commit}').decode().strip() != spec['commit']:
            raise ValueError('Invalid pinned commit')
        directory = out / 'builds' / name
        directory.mkdir(parents=True)
        archive = out / (name + '.tar')
        with archive.open('wb') as stream:
            subprocess.run(['git', '-C', str(ROOT), 'archive', spec['commit']], stdout=stream, check=True)
        subprocess.run(['tar', '-xf', str(archive), '-C', str(directory)], check=True)
        archive.unlink()
        # The archive contains no generated configuration or compiled artifacts.
        steps = [('autoconf', ['autoconf']), ('configure', ['./configure']),
                 ('make', make_command(num_directory, spec['commit']))]
        save(out / (name + '-build-commands.json'), steps)
        for step, command in steps:
            result = invoke(command, directory, 600, out / 'logs' / (name + '-' + step + '.log'))
            if result['returncode'] != 0 or result['status'] == 'timeout':
                raise RuntimeError('Build failed: ' + result['log'])
        binary = directory / 'cubicle.opt'
        if not binary.is_file():
            raise RuntimeError('Native binary missing: ' + str(binary))
        dirs[name] = directory
    return dirs


def prepare(manifest, models, out):
    inputs = {}
    for model in models:
        path = model['path']
        data = git('show', manifest['corpus_commit'] + ':' + path)
        destination = out / 'inputs' / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
        inputs[path] = {'path': str(destination), 'sha256': hashlib.sha256(data).hexdigest()}
    save(out / 'inputs.json', inputs)
    return inputs


def command_for(model, config, dirs, inputs, type_only=False):
    name, tx = CONFIGS[config]
    return [str(dirs[name] / 'cubicle.opt'), *COMMON, *tx, *model['options'],
            *(['-type-only'] if type_only else []), inputs[model['path']]['path']]


def append(out, row):
    with (out / 'runs.jsonl').open('a') as stream:
        stream.write(json.dumps(row) + '\n')


def pilot(manifest, models, dirs, inputs, out, rng):
    results = {}
    for index, model in enumerate(models):
        configs = configurations(model)
        rng.shuffle(configs)
        current = {}
        for config in configs:
            log = out / 'logs' / f'{index:03d}-{config}-type.log'
            row = invoke(command_for(model, config, dirs, inputs, True), out, 30, log, True)
            row.update(model=model['path'], config=config, phase='type', repetition=0)
            append(out, row)
            current[config] = row
        for budget in model['pilot_budgets']:
            pending = [c for c in configs if current[c]['status'] in ('typechecked', 'timeout')]
            if not pending:
                break
            for config in pending:
                log = out / 'logs' / f'{index:03d}-{config}-pilot-{budget}.log'
                row = invoke(command_for(model, config, dirs, inputs), out, budget, log)
                row.update(model=model['path'], config=config, phase='pilot', repetition=0)
                append(out, row)
                current[config] = row
                print(model['path'], config, budget, row['status'], row['verdict'], flush=True)
        # All timed repetitions share this model's largest pilot-attempt budget.
        used = [r['timeout_seconds'] for r in current.values() if r['phase'] == 'pilot']
        common_budget = max(used, default=model['pilot_budgets'][0])
        results[model['path']] = {'budget': common_budget, 'results': current}
        save(out / 'pilot.json', results)
    return results


def measure(models, dirs, inputs, out, pilots, repetitions, rng):
    for rep in range(1, repetitions + 1):
        jobs = [(i, m, c) for i, m in enumerate(models) for c in configurations(m)
                if pilots[m['path']]['results'][c]['status'] == 'completed']
        rng.shuffle(jobs)
        for index, model, config in jobs:
            log = out / 'logs' / f'{index:03d}-{config}-run-{rep}.log'
            row = invoke(command_for(model, config, dirs, inputs), out,
                         pilots[model['path']]['budget'], log)
            row.update(model=model['path'], config=config, phase='measured', repetition=rep)
            append(out, row)
            print(rep, model['path'], config, row['status'], row['verdict'], flush=True)


def summarize(out, models):
    rows = [json.loads(line) for line in (out / 'runs.jsonl').read_text().splitlines()]
    fields = ['model', 'config', 'phase', 'repetition', 'status', 'verdict', 'returncode',
              'wall_seconds', 'timeout_seconds', 'log']
    with (out / 'runs.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(rows)
    summary = []
    for model in models:
        entries = [r for r in rows if r['model'] == model['path']
                   and (r['phase'] != 'type' or r['status'] != 'typechecked')]
        verdicts = {r['verdict'] for r in entries if r['status'] == 'completed'}
        disagreement = len(verdicts) > 1
        expectation_mismatch = bool(model['expected'] and any(v != model['expected'] for v in verdicts))
        for config in configurations(model):
            measured = [r for r in entries if r['config'] == config and r['phase'] == 'measured']
            ok = [r for r in measured if r['status'] == 'completed']
            summary.append({'model': model['path'], 'config': config,
                            'measured_runs': len(measured), 'successful_runs': len(ok),
                            'median_seconds': statistics.median(r['wall_seconds'] for r in ok) if ok else None,
                            'statuses': sorted({r['status'] for r in entries if r['config'] == config}),
                            'verdicts': sorted({r['verdict'] for r in entries if r['config'] == config and r['verdict']}),
                            'verdict_disagreement': disagreement, 'expectation_mismatch': expectation_mismatch,
                            'timing_eligible': bool(measured) and len(ok) == len(measured) and not disagreement and not expectation_mismatch})
    save(out / 'summary.json', summary)
    print('Results:', out, flush=True)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--manifest', type=Path, default=ROOT / 'experiments/bench_manifest.json')
    parser.add_argument('--phase', choices=['plan', 'build', 'pilot', 'run'], default='plan')
    parser.add_argument('--output', type=Path, help='New directory, outside the repository, required for execution')
    parser.add_argument('--models', nargs='+', help='Selected basenames or complete relative paths')
    parser.add_argument('--runs', type=int, default=3)
    parser.add_argument('--seed', type=int, default=2026)
    args = parser.parse_args()
    if args.runs < 1:
        parser.error('--runs must be positive')
    manifest = json.loads(args.manifest.read_text())
    models = select(manifest, args.models)
    if args.phase == 'plan':
        for m in models:
            print(m['path'], m['group'], ' '.join(m['options']) or '(backward defaults)',
                  'pilot ceilings=' + str(m['pilot_budgets']), 'configs=' + ','.join(configurations(m)))
        print('Models:', len(models), 'configuration/model pairs:', sum(len(configurations(m)) for m in models))
        return
    if args.output is None:
        parser.error('--output is required for execution')
    out = args.output.expanduser().resolve()
    if out == ROOT or ROOT in out.parents:
        parser.error('Output must be outside the source checkout')
    out.mkdir(parents=True, exist_ok=False)
    (out / 'logs').mkdir()
    save(out / 'manifest.json', manifest)
    metadata = {'platform': platform.platform(), 'machine': platform.machine(),
                'python': sys.version, 'ocaml': subprocess.check_output(['ocamlopt', '-version']).decode().strip(),
                'opam_switch': subprocess.check_output(['opam', 'switch', 'show']).decode().strip(),
                'common_options': COMMON, 'runs': args.runs, 'seed': args.seed,
                'selected_models': [m['path'] for m in models],
                'timing': 'Direct sequential subprocesses; perf_counter wall time includes spawn/wait overhead; pilot excluded.',
                'argv': sys.argv}
    save(out / 'environment.json', metadata)
    dirs = build(manifest, out)
    save(out / 'binary_hashes.json', {name: hashlib.sha256((d / 'cubicle.opt').read_bytes()).hexdigest() for name, d in dirs.items()})
    if args.phase == 'build':
        print('Builds:', out)
        return
    inputs = prepare(manifest, models, out)
    rng = random.Random(args.seed)
    pilots = pilot(manifest, models, dirs, inputs, out, rng)
    if args.phase == 'run':
        measure(models, dirs, inputs, out, pilots, args.runs, rng)
    summarize(out, models)


if __name__ == '__main__':
    main()
