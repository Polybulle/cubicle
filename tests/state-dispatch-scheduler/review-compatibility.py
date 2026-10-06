#!/usr/bin/env python3
"""Review addition: paired backward-disabled queue/postponement compatibility."""
import argparse
import itertools
import json
from pathlib import Path
from run import REPO, HERE, invoke

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--baseline', type=Path, required=True)
p.add_argument('--output', type=Path, required=True)
a = p.parse_args()
a.output.mkdir(exist_ok=False)
results = []
for mode, strategy, post, deletion in itertools.product(
    [[], ['-tx','none'], ['-tx','fwd'], ['-tx','ignore']],
    ['bfs','dfs','bfsh','dfsh','bfsa','dfsa'], ['0','1','2'], [[],['-nodelete']]):
    flags = ['-nocolor','-depth','20','-nodes','100',*mode,'-search',strategy,
             '-postpone',post,*deletion,'examples/bakery_lamport_bogus.cub']
    pair = [invoke([str(binary.resolve()),*flags], timeout=10)
            for binary in (a.baseline, REPO/'cubicle.opt')]
    results.append(dict(flags=flags, baseline=pair[0], candidate=pair[1],
                        equal=pair[0]['code'] == pair[1]['code'] and pair[0]['output'] == pair[1]['output']))
(a.output/'results.json').write_text(json.dumps(results,indent=2))
assert len(results) == 144
summary = dict(pairs=len(results), equal=sum(r['equal'] for r in results),
               outcomes={k:sum(r['candidate']['outcome'] == k for r in results)
                         for k in ('SAFE','UNSAFE','LIMIT','ERROR','TIMEOUT')})
print(json.dumps(summary))
raise SystemExit(int(not all(r['equal'] for r in results)))
