"""Inspect same-parameter benchmark repetitions without changing measurements."""
import argparse
import collections
import csv
import hashlib
import html
import json
from pathlib import Path
import statistics


def analyze_cell(rows):
    if any(r['command'] != rows[0]['command'] for r in rows):
        raise ValueError('A cell contains different command parameters')
    successful = sorted((r for r in rows if r['status'] == 'completed'),
                        key=lambda r: r['repetition'])
    times = [r['wall_seconds'] for r in successful]
    assessable = len(successful) == len(rows) == 3
    maximum = max(times) if times else None
    minimum = min(times) if times else None
    median = statistics.median(times) if times else None
    others = sorted(times)[:-1]
    reference = statistics.median(others) if others else None
    ratio = maximum / minimum if minimum else None
    slow_ratio = maximum / reference if reference else None
    slow_gap = maximum - reference if reference is not None else None
    return dict(
        assessable=assessable,
        times_seconds=times,
        median_seconds=median,
        min_seconds=minimum,
        max_seconds=maximum,
        max_min_ratio=ratio,
        spread_seconds=maximum - minimum if times else None,
        slow_other_two_ratio=slow_ratio,
        slow_other_two_gap_seconds=slow_gap,
        slow_repetition=max(successful, key=lambda r: r['wall_seconds'])['repetition'] if successful else None,
        isolated_slow_candidate=bool(assessable and slow_ratio >= 2 and slow_gap >= .1),
        long_run_variability=bool(assessable and median >= 1 and ratio >= 1.1),
        counters_identical=all(r['stats'] == successful[0]['stats'] for r in successful) if successful else None,
        verdicts_identical=len({r['verdict'] for r in successful}) <= 1,
        statuses=dict(collections.Counter(r['status'] for r in rows)),
        repetitions=[dict(repetition=r['repetition'], wall_seconds=r['wall_seconds'],
                          status=r['status'], verdict=r['verdict'],
                          returncode=r['returncode'], log='logs/' + Path(r['log']).name,
                          stats=r['stats']) for r in rows],
    )


def report_page(report):
    escape = html.escape
    name = escape(report['source'])
    def table(entries):
        body = []
        for cell in entries:
            times = ', '.join(f'{t:.6f}' for t in cell['times_seconds'])
            logs = ' '.join(f'<a href="{escape(r["log"])}">r{r["repetition"]}</a>'
                            for r in cell['repetitions'])
            body.append('<tr>' + ''.join(f'<td>{v}</td>' for v in (
                escape(cell['model']), escape(cell['config']), times,
                f'{cell["max_min_ratio"]:.3f}', f'{cell["spread_seconds"]:.6f}', logs)) + '</tr>')
        return '<div class="scroll"><table><thead><tr><th>Model</th><th>Configuration</th><th>r1, r2, r3 (s)</th><th>max/min</th><th>Spread (s)</th><th>Logs</th></tr></thead><tbody>' + ''.join(body) + '</tbody></table></div>'
    candidates = sorted((c for c in report['cells'] if c['isolated_slow_candidate']),
                        key=lambda c: c['slow_other_two_ratio'], reverse=True)
    varying = sorted((c for c in report['cells'] if c['long_run_variability']),
                     key=lambda c: c['max_min_ratio'], reverse=True)
    return ('<!doctype html><html lang="en"><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            f'<title>Cubicle {name} · Repetition spread</title><style>'
            'body{font:16px/1.5 system-ui;max-width:1300px;margin:30px auto;padding:0 20px;color:#172a34;background:#f4f6f2}'
            'a{color:#187567}.scroll{overflow:auto}table{border-collapse:collapse;background:white;width:100%;font-size:14px}'
            'td,th{text-align:left;padding:10px;border-bottom:1px solid #dce3df}td{white-space:nowrap}'
            f'</style><h1>{name}: same-parameter repetition spread</h1>'
            f'<p><a href="../{name}.html">Benchmark dashboard</a> · <a href="outliers.json">Full JSON</a> · <a href="repetition-spread.csv">All cells (CSV)</a></p>'
            f'<p>{report["assessable_cells"]} cells have three completed measurements. '
            f'{report["unassessable_cells"]} non-completing cells are not assessed for timing variation.</p>'
            '<p>Heuristic review flags, not statistical outlier tests. No measurements were removed and no medians changed. '
            f'Benchmark workers: {report.get("workers", "not recorded")}. Identical recorded search counters do not establish the cause of timing changes.</p>'
            f'<h2>Isolated slow candidates ({len(candidates)})</h2>'
            '<p>The slowest repetition is at least twice the median of the other two, and at least 0.1 seconds slower.</p>'
            + table(candidates)
            + f'<h2>Long-run variability ({len(varying)})</h2>'
            '<p>Median at least one second and max/min at least 1.10. These are spread flags, not necessarily isolated outliers.</p>'
            + table(varying)
            + f'<p>Commands match within every cell. Recorded search counters match in '
            f'{sum(c["assessable"] and c["counters_identical"] for c in report["cells"])}/'
            f'{report["assessable_cells"]} assessed cells. Verdicts match in '
            f'{sum(c["assessable"] and c["verdicts_identical"] for c in report["cells"])}/'
            f'{report["assessable_cells"]} assessed cells.</p></html>')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    args = parser.parse_args()
    source = args.source.resolve()
    raw = (source / 'runs.jsonl').read_bytes()
    rows = [json.loads(line) for line in raw.splitlines()]
    groups = collections.defaultdict(list)
    for row in rows:
        if row['phase'] == 'measured':
            groups[row['model'], row['config']].append(row)
    cells = [dict(model=key[0], config=key[1], **analyze_cell(value))
             for key, value in sorted(groups.items())]
    report = dict(source=source.name, runs_sha256=hashlib.sha256(raw).hexdigest(),
                  workers=json.loads((source / 'environment.json').read_text()).get('workers', 1),
                  method='Review flags only; isolated slow: max >= 2 * median(other two) and excess >= 0.1s; long variability: median >= 1s and max/min >= 1.10. Three completed repetitions required. No exclusions or rewritten medians.',
                  assessable_cells=sum(c['assessable'] for c in cells),
                  unassessable_cells=sum(not c['assessable'] for c in cells),
                  isolated_slow_candidates=sum(c['isolated_slow_candidate'] for c in cells),
                  long_run_variability_cells=sum(c['long_run_variability'] for c in cells),
                  cells=cells)
    (source / 'outliers.json').write_text(json.dumps(report, indent=2) + '\n')
    fields = ['model', 'config', 'assessable', 'times_seconds', 'median_seconds',
              'min_seconds', 'max_seconds', 'max_min_ratio', 'spread_seconds',
              'slow_other_two_ratio', 'slow_repetition', 'isolated_slow_candidate',
              'long_run_variability', 'counters_identical', 'verdicts_identical']
    with (source / 'repetition-spread.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(cells)
    (source / 'outliers.html').write_text(report_page(report))
    print(json.dumps({k: v for k, v in report.items() if k != 'cells'}, indent=2))


if __name__ == '__main__':
    main()
