#!/usr/bin/env python3
"""Review addition: aggregate frozen raw samples, including locale decimal commas.
Does not modify the frozen runner or its initial results. Run from repo root.
"""
import json
import re
import statistics as st
from pathlib import Path

HERE = Path(__file__).resolve().parent
SOURCE = HERE / '.local/controlled-preinstall'
runs = [json.loads(line) for line in (SOURCE/'runs.jsonl').read_text().splitlines()]
assert len(runs) == 384
rows = []
for model, mode in dict.fromkeys((r['model'], r['mode']) for r in runs):
    group = [r for r in runs if r['model'] == model and r['mode'] == mode and not r['warmup']]
    data = {}
    for label in ('baseline', 'candidate'):
        samples = [r for r in group if r['label'] == label]
        assert len(samples) == 7
        process_times = []
        for r in samples:
            match = re.search(r'^\s*([\d.,]+) real\s+([\d.,]+) user\s+([\d.,]+) sys', r['output'], re.M)
            assert match
            process_times.append([float(x.replace(',', '.')) for x in match.groups()])
        data[label] = dict(median_wall=st.median(r['seconds'] for r in samples),
                           wall_range=[min(r['seconds'] for r in samples), max(r['seconds'] for r in samples)],
                           median_rss_bytes=st.median(r['rss_bytes'] for r in samples),
                           outcomes=sorted(set((r['outcome'], r['code']) for r in samples)),
                           counts=samples[0]['counts'], process_times=process_times,
                           error_trace=re.findall(r'^Error trace:.*(?:\n[ \t]+\S.*)*', samples[0]['output'], re.M))
        assert all(r['counts'] == samples[0]['counts'] for r in samples)
    def verifier_output(r):
        # Preserve every verifier line, including wrapped traces and diagnostics.
        return re.split(r'^\s*[\d.,]+ real\s+[\d.,]+ user\s+[\d.,]+ sys', r['output'], flags=re.M)[0]
    pairs = [[next(r for r in group if r['rep'] == i and r['label'] == label)
              for label in ('baseline', 'candidate')] for i in range(7)]
    rows.append(dict(model=model, mode=mode, **data,
                     full_verifier_output_equal=all(verifier_output(b) == verifier_output(c) for b,c in pairs),
                     paired_wall_deltas=[c['seconds']-b['seconds'] for b,c in pairs]))
assert len(rows) == 24
for r in rows:
    ordinary = next(x for x in rows if x['model'] == r['model'] and x['mode'] == 'ordinary')
    r['candidate_overhead_vs_ordinary_percent'] = 100*(r['candidate']['median_wall']/ordinary['candidate']['median_wall']-1)
    r['same_mode_delta_percent'] = 100*(r['candidate']['median_wall']/r['baseline']['median_wall']-1)
    r['same_mode_performance_comparable'] = all(x[0] in ('SAFE','UNSAFE') for label in ('baseline','candidate') for x in r[label]['outcomes'])
(HERE/'review-measurements.json').write_text(json.dumps(dict(metadata=json.loads((SOURCE/'metadata.json').read_text()), groups=rows),indent=2)+'\n')
for r in rows:
    print(Path(r['model']).stem, r['mode'], 'full-output-equal', r['full_verifier_output_equal'],
          'delta%', round(r['same_mode_delta_percent'],2), 'candidate-overhead%', round(r['candidate_overhead_vs_ordinary_percent'],2),
          'ranges', r['baseline']['wall_range'], r['candidate']['wall_range'])
