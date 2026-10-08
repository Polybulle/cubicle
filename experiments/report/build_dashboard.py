"""Build a dependency-free, offline benchmark explorer without modifying results."""
import argparse
import copy
import json
import statistics
from pathlib import Path

ROOT = Path(__file__).resolve().parent


def load_data(source):
    def read(name):
        return json.loads((source / name).read_text())
    manifest = read('manifest.json')
    runs = [json.loads(line) for line in (source / 'runs.jsonl').read_text().splitlines() if line.strip()]
    for run in runs:
        archive = Path(run.get('reused_from', source))
        log = archive / 'logs' / Path(run['log']).name
        run['log_url'] = log.relative_to(source.parent).as_posix() if log.is_file() else None
    return dict(name=source.name, models=manifest['models'], summary=read('summary.json'),
                runs=runs, environment=read('environment.json'), builds=manifest['builds'],
                policy=manifest['policy'], hashes=read('binary_hashes.json'),
                repetitions_url=(source.name + '/outliers.html') if (source / 'outliers.html').is_file() else None,
                input_hashes={p: s['sha256'] for p, s in read('inputs.json').items()})


def integrate_updates(base, update):
    key = lambda entry: (entry['model'], entry['config'])
    existing = {key(s) for s in base['summary']}
    replacements = {key(s): s for s in update['summary']}
    if len(replacements) != len(update['summary']) or not replacements or not set(replacements) <= existing:
        raise ValueError('Overlay contains duplicate, empty, or unknown cells')
    models = {m['path']: m for m in base['models']}
    if {model for model, config in replacements} != {m['path'] for m in update['models']}:
        raise ValueError('Overlay models do not match its cells')
    for model in update['models']:
        path = model['path']
        if path not in models or base['input_hashes'][path] != update['input_hashes'].get(path):
            raise ValueError('Overlay input differs: ' + path)
        if any(model.get(field) != models[path].get(field) for field in ('options', 'group', 'expected', 'timeout_seconds')):
            raise ValueError('Overlay recipe differs: ' + path)
    if {key(r) for r in update['runs']} != set(replacements):
        raise ValueError('Overlay records do not match its cells')
    result = copy.deepcopy(base)
    result['original'] = copy.deepcopy(base.get('original', {'summary': base['summary'], 'runs': base['runs']}))
    result['overlay'] = {field: copy.deepcopy(update[field]) for field in ('name', 'environment', 'builds', 'hashes', 'policy')}
    result['overlay']['cells'] = sorted(replacements)
    previous = base.get('overlays', [])
    if any(s['name'] == update['name'] for s in previous):
        raise ValueError('Duplicate overlay source: ' + update['name'])
    result['overlays'] = copy.deepcopy(previous) + [copy.deepcopy(result['overlay'])]
    result['runs'] = [r for r in result['runs'] if key(r) not in replacements]
    result['runs'] += [dict(copy.deepcopy(r), updated_from=update['name']) for r in update['runs']]
    for index, entry in enumerate(result['summary']):
        if key(entry) not in replacements:
            continue
        rows = [r for r in result['runs'] if key(r) == key(entry)]
        measured = [r for r in rows if r['phase'] == 'measured']
        completed = [r for r in measured if r['status'] == 'completed']
        entry = dict(copy.deepcopy(replacements[key(entry)]), updated_from=update['name'])
        entry.update(measured_runs=len(measured), successful_runs=len(completed),
                     median_seconds=statistics.median(r['wall_seconds'] for r in completed) if completed else None,
                     statuses=sorted({r['status'] for r in rows}),
                     verdicts=sorted({r['verdict'] for r in rows if r['verdict']}))
        result['summary'][index] = entry
    # Verdict eligibility belongs to the full model comparison, not the subset.
    for path in {model for model, config in replacements}:
        verdicts = {r['verdict'] for r in result['runs'] if r['model'] == path and r['status'] == 'completed'}
        disagreement = len(verdicts) > 1
        mismatch = bool(models[path]['expected'] and any(v != models[path]['expected'] for v in verdicts))
        for entry in result['summary']:
            if entry['model'] == path:
                entry.update(verdict_disagreement=disagreement, expectation_mismatch=mismatch,
                             timing_eligible=bool(entry['measured_runs']) and
                             entry['successful_runs'] == entry['measured_runs'] and not disagreement and not mismatch)
    return result


def render(data):
    payload = json.dumps(data, ensure_ascii=False).replace('<', '\\u003c')
    return (ROOT / 'dashboard.html').read_text().replace('__BENCHMARK_DATA__', payload)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=Path.cwd() / 'run-02')
    parser.add_argument('--overlay', type=Path, nargs='+', help='Overlay focused archives in order; preserve the original view')
    parser.add_argument('--output', type=Path, default=Path.cwd() / 'index.html')
    args = parser.parse_args()
    data = load_data(args.source.resolve())
    for overlay in args.overlay or []:
        data = integrate_updates(data, load_data(overlay.resolve()))
    output = args.output
    output.write_text(render(data))
    print(f'Built {output}: {len(data["models"])} models, {len(data["summary"])} configurations, {len(data["runs"])} execution records')
