#!/usr/bin/env python3
"""Curated Cubicle comparison. Uses isolated sources, never checkout/reset.

Default is a read-only plan. Build/run require a new output directory.
Timing uses perf_counter around direct subprocesses, not a Python timeout wrapper
inside hyperfine. Every measured run retains its verdict and complete output.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor, wait, FIRST_COMPLETED
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import random
import re
import signal
import shutil
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
COMMON = ['-nocolor', '-solver', 'alt-ergo']


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def progress(phase, done, total, current=''):
    filled = 20 * done // total if total else 20
    bar = '#' * filled + '-' * (20 - filled)
    text = f'{phase} [{bar}] {done}/{total} {current}'.rstrip()
    if sys.stdout.isatty():
        print('\r\033[2K' + text, end='\n' if done == total else '', flush=True)
    else:
        print(text, flush=True)


def git(*args):
    return subprocess.check_output(['git', '-C', str(ROOT), *args])


def configurations(model):
    available = ['tetra-all', 'old-all'] if model['group'] == 'transaction' else list(CONFIGS)
    selected = model.get('configs', available)
    if not selected or len(selected) != len(set(selected)) or any(c not in available for c in selected):
        raise ValueError('Invalid configuration subset: ' + repr(selected))
    return selected


def classify(text, code, timed_out=False, type_only=False):
    if timed_out:
        return 'timeout', None
    if code is not None and code < 0:
        return 'crash', None
    # Native verdicts require SAFE/0 or UNSAFE/1; other exits are errors.
    verdicts = set(re.findall(r'The system is (SAFE|UNSAFE)\b', text))
    if re.search(r'^UNSAFE\b', text, re.MULTILINE):
        verdicts.add('UNSAFE')
    if len(verdicts) == 1:
        verdict = next(iter(verdicts))
        if code != (0 if verdict == 'SAFE' else 1):
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


def snapshot_working_tree(out):
    snapshot = out / 'source'
    snapshot.mkdir()
    hashes = {}
    paths = sorted(set(git('ls-files', '-z', '--cached', '--others', '--exclude-standard').split(b'\0')) - {b''})
    for raw in paths:
        path = os.fsdecode(raw)
        source = ROOT / path
        if not source.exists():
            continue  # Tracked deletion in the working tree.
        destination = snapshot / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
        hashes[path] = hashlib.sha256(destination.read_bytes()).hexdigest()
    (out / 'working-tree.patch').write_bytes(git('diff', '--binary', 'HEAD'))
    save(out / 'working-tree.json', {
        'base_commit': git('rev-parse', 'HEAD').decode().strip(),
        'status': git('status', '--short').decode(), 'sha256': hashes})
    return snapshot


def build(manifest, out, snapshot=None):
    dirs = {}
    done = 0
    total = 3 * len(manifest['builds'])
    num_directory = subprocess.check_output(['ocamlfind', 'query', 'num']).decode().strip()
    for name, spec in manifest['builds'].items():
        if git('rev-parse', spec['commit'] + '^{commit}').decode().strip() != spec['commit']:
            raise ValueError('Invalid pinned commit')
        directory = out / 'builds' / name
        if name == 'tetra' and snapshot is not None:
            shutil.copytree(snapshot, directory)
        else:
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
            progress('Build', done, total, name + ' ' + step)
            result = invoke(command, directory, 600, out / 'logs' / (name + '-' + step + '.log'))
            if result['returncode'] != 0 or result['status'] == 'timeout':
                raise RuntimeError('Build failed: ' + result['log'])
            done += 1
        binary = directory / 'cubicle.opt'
        if not binary.is_file():
            raise RuntimeError('Native binary missing: ' + str(binary))
        dirs[name] = directory
    progress('Build', done, total)
    return dirs


def prepare(manifest, models, out, snapshot=None):
    inputs = {}
    for model in models:
        path = model['path']
        data = ((snapshot / path).read_bytes() if snapshot is not None else
                git('show', manifest['corpus_commit'] + ':' + path))
        destination = out / 'inputs' / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
        inputs[path] = {'path': str(destination), 'sha256': hashlib.sha256(data).hexdigest()}
    save(out / 'inputs.json', inputs)
    return inputs


def command_for(model, config, dirs, inputs):
    name, tx = CONFIGS[config]
    return [str(dirs[name] / 'cubicle.opt'), *COMMON, *tx, *model['options'],
            '-j', '1', inputs[model['path']]['path']]


def append(out, row):
    with (out / 'runs.jsonl').open('a') as stream:
        stream.write(json.dumps(row) + '\n')


def execute_jobs(jobs, out, workers, phase):
    def run(job):
        row = invoke(job['command'], out, job['timeout_seconds'], Path(job['log']),
                     job['phase'] == 'type')
        row.update({key: job[key] for key in ('model', 'config', 'phase', 'repetition')})
        return row

    remaining = iter(jobs)
    done = 0
    with ThreadPoolExecutor(max_workers=workers) as pool:
        pending = set()
        while True:
            while len(pending) < workers:
                job = next(remaining, None)
                if job is None:
                    break
                progress(phase, done, len(jobs),
                         f'{job["model"]} {job["config"]} {job["timeout_seconds"]}s')
                pending.add(pool.submit(run, job))
            if not pending:
                break
            finished, pending = wait(pending, return_when=FIRST_COMPLETED)
            for future in finished:
                row = future.result()
                append(out, row)  # Only the coordinator writes shared records.
                done += 1
                yield row
    progress(phase, done, len(jobs))


def reuse_results(source, manifest, models, inputs, out, repetitions):
    """Reuse pinned comparison cells, retaining their original phases and argv."""
    previous = json.loads((source / 'manifest.json').read_text())
    old_inputs = json.loads((source / 'inputs.json').read_text())
    rows = [json.loads(line) for line in (source / 'runs.jsonl').read_text().splitlines()]
    reused = set()
    for model in models:
        path = model['path']
        for config in configurations(model):
            build_name, tx = CONFIGS[config]
            if build_name == 'tetra':
                continue
            if previous['builds'][build_name]['commit'] != manifest['builds'][build_name]['commit']:
                continue
            if old_inputs.get(path, {}).get('sha256') != inputs[path]['sha256']:
                continue
            cell = [r for r in rows if r['model'] == path and r['config'] == config]
            selected = sorted((r for r in cell if r['phase'] == 'measured'),
                              key=lambda r: r['repetition'])[:repetitions]
            if selected:
                if [r['repetition'] for r in selected] != list(range(1, len(selected) + 1)):
                    continue
                if len(selected) < repetitions and all(r['status'] == 'completed' for r in selected):
                    continue
            else:
                # Keep historical failures in their original phase, never as timings.
                selected = [r for r in cell if r['phase'] == 'pilot' and r['status'] != 'completed']
                if not selected:
                    selected = [r for r in cell if r['phase'] == 'type' and r['status'] == 'unsupported']
                if not selected:
                    continue
                selected = selected[-1:]
            expected = COMMON + tx + model['options']
            if any(r['timeout_seconds'] != model['timeout_seconds'] for r in selected if r['phase'] != 'type'):
                continue
            if any([arg for arg in r['command'][1:-1] if arg != '-type-only']
                   not in (expected, expected + ['-j', '1']) for r in selected):
                continue
            for row in selected:
                log = source / 'logs' / Path(row['log']).name
                if not log.is_file():
                    raise ValueError('Missing reused log: ' + str(log))
                row = dict(row, log=str(log), reused_from=str(source))
                append(out, row)
            reused.add((path, config))
    save(out / 'reuse.json', {'source': str(source), 'cells': sorted(reused),
                             'note': 'Historical measurements; not contemporaneous with this run.'})
    return reused


def measure(models, dirs, inputs, out, repetitions, rng, workers=1, reused=()):
    cells = [(i, m, c) for i, m in enumerate(models) for c in configurations(m)
             if (m['path'], c) not in reused]
    for rep in range(1, repetitions + 1):
        if not cells:
            break
        ordered = list(cells)
        rng.shuffle(ordered)
        jobs = [dict(command=command_for(m, c, dirs, inputs),
                     timeout_seconds=m['timeout_seconds'],
                     log=str(out / 'logs' / f'{i:03d}-{c}-run-{rep}.log'),
                     model=m['path'], config=c, phase='measured', repetition=rep)
                for i, m, c in ordered]
        completed = set()
        for row in execute_jobs(jobs, out, workers, f'Runs {rep}/{repetitions}'):
            if row['status'] == 'completed':
                completed.add((row['model'], row['config']))
        cells = [(i, m, c) for i, m, c in cells if (m['path'], c) in completed]


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
    parser.add_argument('--phase', choices=['plan', 'build', 'run'], default='plan')
    parser.add_argument('--output', type=Path, help='New directory, outside the repository, required for execution')
    parser.add_argument('--models', nargs='+', help='Selected basenames or complete relative paths')
    parser.add_argument('--runs', type=int, default=3)
    parser.add_argument('--seed', type=int, default=2026)
    parser.add_argument('--jobs', type=int, choices=range(1, 7), default=1,
                        help='Concurrent sequential Cubicle processes (1–6; no CPU pinning)')
    parser.add_argument('--reuse-from', type=Path,
                        help='Reuse compatible baseline/old measurements from an earlier run')
    parser.add_argument('--working-tree', action='store_true',
                        help='Snapshot current tracked/nonignored files for the Tetra build and shared corpus')
    args = parser.parse_args()
    if args.runs < 1:
        parser.error('--runs must be positive')
    manifest = json.loads(args.manifest.read_text())
    models = select(manifest, args.models)
    for model in models:
        if model['timeout_seconds'] not in (5, 100, 450):
            parser.error('Model timeout must be 5, 100, or 450 seconds')
        try:
            configurations(model)
        except ValueError as error:
            parser.error(str(error))
    if args.working_tree:
        head = git('rev-parse', 'HEAD').decode().strip()
        manifest['builds']['tetra'] = {'ref': 'working-tree', 'commit': head,
                                       'source': 'source/', 'provenance': 'working-tree.json'}
        manifest['corpus_ref'] = 'working-tree'
        manifest['corpus_commit'] = head
    if args.phase == 'plan':
        for m in models:
            print(m['path'], m['group'], ' '.join(m['options']) or '(backward defaults)',
                  'timeout=' + str(m['timeout_seconds']), 'configs=' + ','.join(configurations(m)))
        print('Models:', len(models), 'configuration/model pairs:', sum(len(configurations(m)) for m in models))
        print('Workers:', args.jobs, 'Cubicle cores per process: 1; CPU affinity: OS-managed')
        return
    if args.output is None:
        parser.error('--output is required for execution')
    out = args.output.expanduser().resolve()
    if out == ROOT or ROOT in out.parents:
        parser.error('Output must be outside the source checkout')
    out.mkdir(parents=True, exist_ok=False)
    (out / 'logs').mkdir()
    snapshot = snapshot_working_tree(out) if args.working_tree else None
    save(out / 'manifest.json', manifest)
    metadata = {'platform': platform.platform(), 'machine': platform.machine(),
                'python': sys.version, 'ocaml': subprocess.check_output(['ocamlopt', '-version']).decode().strip(),
                'opam_switch': subprocess.check_output(['opam', 'switch', 'show']).decode().strip(),
                'common_options': COMMON, 'runs': args.runs, 'seed': args.seed,
                'workers': args.jobs, 'cubicle_cores': 1, 'cpu_affinity': 'OS-managed, not pinned',
                'selected_models': [m['path'] for m in models],
                'timing': f'Up to {args.jobs} concurrent single-core Cubicle subprocesses; '
                          'perf_counter wall time includes spawn/wait but excludes queue time; '
                          'No separate typechecks or pilots; first attempts count as measurements. '
                          'Non-completing cells are not retried. CPU placement is OS-managed. '
                          'Concurrent timings may include contention.',
                'argv': sys.argv}
    if args.reuse_from:
        metadata['timing'] += (' Compatible baseline/old results are historical, reused from '
                               + str(args.reuse_from) + '; their original concurrency and phases apply.')
    save(out / 'environment.json', metadata)
    inputs = prepare(manifest, models, out, snapshot)
    reused = (reuse_results(args.reuse_from.expanduser().resolve(), manifest, models,
                            inputs, out, args.runs) if args.reuse_from else set())
    needed = {CONFIGS[c][0] for m in models for c in configurations(m)
              if (m['path'], c) not in reused}
    build_manifest = dict(manifest, builds={k: v for k, v in manifest['builds'].items() if k in needed})
    print('Reused cells:', len(reused), '; builds needed:', ', '.join(sorted(needed)), flush=True)
    dirs = build(build_manifest, out, snapshot)
    save(out / 'binary_hashes.json', {name: hashlib.sha256((d / 'cubicle.opt').read_bytes()).hexdigest() for name, d in dirs.items()})
    if args.phase == 'build':
        print('Builds:', out)
        return
    rng = random.Random(args.seed)
    measure(models, dirs, inputs, out, args.runs, rng, args.jobs, reused)
    summarize(out, models)


if __name__ == '__main__':
    main()
