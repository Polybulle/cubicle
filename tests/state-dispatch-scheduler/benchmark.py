#!/usr/bin/env python3
"""Paired macOS performance capture; no numerical acceptance threshold implied.
Build both versions BEFORE use; do not run concurrently with builds/benchmarks.
Each process has explicit node/depth and external wall limits. No RSS cap is set.
"""
import argparse
import hashlib
import json
import platform
import random
import re
import statistics
import subprocess
from pathlib import Path
from typing import Any
from run import HERE, REPO, invoke

MODES = {'ordinary': [], 'none': ['-tx','none'], 'fwd': ['-tx','fwd'],
         'ignore': ['-tx','ignore'], 'bwd': ['-tx','bwd'], 'bare': ['-tx']}
MODELS = ['examples/bakery.cub', 'examples/german.cub',
          'examples/bakery_lamport_bogus.cub', 'examples/swimming_pool.cub']

def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def observations(r):
    text = r['output']
    counts = dict(re.findall(r'^(Number of visited nodes|Fixpoints|Number of solver calls|Max Number of processes|Number of deleted nodes|Restarts)\s*:\s*(\d+)', text, re.M))
    traces = re.findall(r'^(?:Error trace:|Unsafe trace:|node \d+:).*$', text, re.M)
    rss = re.search(r'^\s*(\d+)\s+maximum resident set size', text, re.M)
    times = re.search(r'^\s*([\d.]+) real\s+([\d.]+) user\s+([\d.]+) sys', text, re.M)
    return dict(counts=counts, traces=traces,
                rss_bytes=int(rss[1]) if rss else None,
                process_seconds=list(map(float,times.groups())) if times else None)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--baseline', type=Path, required=True)
    p.add_argument('--candidate', type=Path, required=True)
    p.add_argument('--baseline-revision', required=True)
    p.add_argument('--candidate-revision', required=True)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--repetitions', type=int, default=7)
    p.add_argument('--timeout', type=float, default=120)
    p.add_argument('--runner-smoke', action='store_true', help='one tiny model; NOT performance evidence')
    a = p.parse_args()
    if a.repetitions < 1 or a.timeout <= 0:
        p.error('positive repetitions and timeout required')
    a.output.mkdir(parents=True, exist_ok=False)
    binaries = {'baseline':a.baseline.resolve(), 'candidate':a.candidate.resolve()}
    metadata = dict(platform=platform.platform(), machine=platform.machine(),
                    revisions={'baseline':a.baseline_revision, 'candidate':a.candidate_revision},
                    binaries={k:dict(path=str(v), sha256=digest(v)) for k,v in binaries.items()},
                    repetitions=a.repetitions, wall_limit=a.timeout, depth=100, nodes=100000,
                    seed=1663, runner_smoke=a.runner_smoke,
                    environment={key:subprocess.run(cmd,capture_output=True,text=True).stdout
                                 for key,cmd in {'cpu':['sysctl','-n','machdep.cpu.brand_string'],
                                                 'memory':['sysctl','-n','hw.memsize'],
                                                 'load':['uptime']}.items()})
    models = [str(HERE/'models/ordinary-unsafe.cub')] if a.runner_smoke else MODELS
    metadata['models'] = {m:digest(REPO/m) for m in models}
    (a.output/'metadata.json').write_text(json.dumps(metadata,indent=2))
    rng = random.Random(1663)
    results = []
    with (a.output/'runs.jsonl').open('w') as records:
        for model in models:
            for mode, flags in MODES.items():
                for rep in range(-1,a.repetitions):  # one warmup of each binary
                    order = list(binaries)
                    rng.shuffle(order)
                    for label in order:
                        cmd = ['/usr/bin/time','-l',str(binaries[label]),'-nocolor',
                               '-depth','100','-nodes','100000',*flags,str(REPO/model)]
                        r = invoke(cmd, timeout=a.timeout)
                        r.update(observations(r))
                        r.update(label=label,model=model,mode=mode,rep=rep,warmup=rep<0)
                        records.write(json.dumps(r)+'\n')
                        records.flush()
                        results.append(r)
    summary = []
    for model in models:
        for mode in MODES:
            group = [r for r in results if r['model']==model and r['mode']==mode and not r['warmup']]
            by_label = {k:[r for r in group if r['label']==k] for k in binaries}
            row: dict[str, Any] = dict(model=model,mode=mode)
            for label, rows in by_label.items():
                times = [r['seconds'] for r in rows]
                memory = [r['rss_bytes'] for r in rows if r['rss_bytes'] is not None]
                row[label] = dict(outcomes=[(r['outcome'],r['code']) for r in rows],
                                  median_wall=statistics.median(times),
                                  min_wall=min(times),max_wall=max(times),
                                  median_rss=statistics.median(memory) if memory else None,
                                  counts=[r['counts'] for r in rows], traces=[r['traces'] for r in rows])
            b,c = row['baseline'],row['candidate']
            row['wall_delta_seconds'] = c['median_wall']-b['median_wall']
            row['wall_delta_percent'] = 100*(c['median_wall']/b['median_wall']-1)
            row['rss_delta_bytes'] = c['median_rss']-b['median_rss'] if b['median_rss'] is not None and c['median_rss'] is not None else None
            row['behavior_equal'] = all(b[key]==c[key] for key in ['outcomes','counts','traces'])
            row['verdict_samples_only'] = all(r['outcome'] in ['SAFE','UNSAFE'] for r in group)
            summary.append(row)
    (a.output/'summary.json').write_text(json.dumps(summary,indent=2))
    print(json.dumps({'runs':len(results),'groups':len(summary),
                      'behavior_equal':sum(r['behavior_equal'] for r in summary),
                      'warning':'No automatic performance acceptance. Inspect failures, noise and raw results.'}))

if __name__ == '__main__':
    main()
