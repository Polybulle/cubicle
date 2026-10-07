#!/usr/bin/env python3
"""Check neutral ordinary nodes and bounded ordinary safe/unsafe searches."""
import json
from pathlib import Path
import re
import runpy

HERE = Path(__file__).resolve().parent
run = runpy.run_path(str(HERE.parent / 'neutral-candidates/run.py'))['run']
for mode in ('none', 'bwd', 'all'):
    code, output = run([str(HERE / '.local/check.opt'), '-nocolor', '-tx', mode,
                        str(HERE / 'model.cub')])
    assert code == 0 and 'PASS ordinary neutral predecessor' in output, output
records = []
for model, expected in (('model.cub', 'SAFE'), ('unsafe.cub', 'UNSAFE')):
    for search in ('bfs', 'dfs'):
        for postpone in (0, 1, 2):
            for deletion in ([], ['-nodelete']):
                command = ['./cubicle.opt', '-nocolor', '-tx', 'none',
                           '-search', search, '-postpone', str(postpone),
                           '-nodes', '200', *deletion, str(HERE / model)]
                code, output = run(command)
                if expected == 'SAFE':
                    assert code == 0 and 'The system is SAFE' in output, output
                else:
                    assert code == 1 and re.search(r'^UNSAFE\b', output, re.M), output
                records.append(dict(command=command, code=code, expected=expected, output=output))
(HERE / '.local/results.json').write_text(json.dumps(records, indent=2) + '\n')
print('PASS', len(records), 'ordinary safe/unsafe integrations')
