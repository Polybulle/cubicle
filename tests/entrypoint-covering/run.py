#!/usr/bin/env python3
"""Exercise entry-only skipping and retain recursive-entry convergence."""
from pathlib import Path
import runpy

HERE = Path(__file__).resolve().parent
run = runpy.run_path(str(HERE.parent / 'neutral-candidates/run.py'))['run']
model = str(HERE / 'model.cub')
code, output = run([str(HERE / '.local/check.opt'), '-tx', 'bwd', '-nocolor', model])
assert code == 0 and 'PASS entrypoint covering policy and scheduler' in output, output
print('PASS entrypoint covering policy and scheduler')
for mode in ('bwd', 'all'):
    for search in ('bfs', 'dfs'):
        for postpone in (0, 1, 2):
            code, output = run(['./cubicle.opt', '-tx', mode, '-search', search,
                                '-postpone', str(postpone), '-nodes', '100',
                                '-nocolor', model])
            assert code == 0 and 'The system is SAFE' in output, output
            print(f'PASS recursive entry mode={mode} search={search} postpone={postpone}')
