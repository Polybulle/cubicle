#!/usr/bin/env python3
"""Check ordinary-mode predecessor and search equivalence under bounded runs."""
import json
from pathlib import Path
import re
import runpy

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
run = runpy.run_path(str(HERE.parent / 'neutral-candidates/run.py'))['run']
records = []
outputs = []
for mode in ('none', 'fwd', 'bwd', 'all'):
    command = [str(HERE / '.local/check.opt'), '-nocolor', '-tx', mode,
               str(HERE / 'model.cub')]
    code, output = run(command)
    assert code == 0 and 'PASS ordinary predecessor' in output, output
    outputs.append(output)
assert all(o == outputs[0] for o in outputs), 'ordinary predecessor order or witnesses changed'
print('PASS ordinary pre-image order, bindings, and history in all four modes')
for model, expected in (('model.cub', 'SAFE'), ('unsafe.cub', 'UNSAFE')):
    for search in ('bfs', 'dfs'):
        for postpone in (0, 1, 2):
            for deletion in ([], ['-nodelete']):
                reference = None
                for mode in ('none', 'fwd', 'bwd', 'all'):
                    command = ['./cubicle.opt', '-nocolor', '-tx', mode,
                               '-search', search, '-postpone', str(postpone),
                               '-nodes', '200', *deletion, str(HERE / model)]
                    code, output = run(command)
                    if expected == 'SAFE':
                        assert code == 0 and 'The system is SAFE' in output, output
                    else:
                        assert code == 1 and re.search(r'^UNSAFE\b', output, re.M), output
                    stats = dict(re.findall(r'^\s*([^\n:]+?)\s*:\s*(\d+)\s*$', output, re.M))
                    trace = [s.strip() for s in output.splitlines()
                             if '->' in s or 'Unsafe trace:' in s]
                    current = stats, trace
                    if reference is None:
                        reference = current
                    else:
                        assert current == reference, (command, current, reference)
                    records.append(dict(command=command, code=code, expected=expected,
                                        stats=stats, output=output))
    print('PASS', model, 'mode-equivalent counters and traces across search/postponement/deletion')
for model, expected in (('flash_nodata.cub', 'SAFE'), ('flash_buggy.cub', 'UNSAFE')):
    reference = None
    for mode in ('none', 'fwd', 'bwd', 'all'):
        command = ['./cubicle.opt', '-nocolor', '-solver', 'alt-ergo', '-tx', mode,
                   '-brab', '2', '-forward-depth', '6', str(ROOT / 'examples' / model)]
        code, output = run(command)
        if expected == 'SAFE':
            assert code == 0 and 'The system is SAFE' in output, output
        else:
            assert code == 1 and re.search(r'^UNSAFE\b', output, re.M), output
        stats = dict(re.findall(r'^\s*([^\n:]+?)\s*:\s*(\d+)\s*$', output, re.M))
        trace = [s.strip() for s in output.splitlines() if '->' in s or 'Unsafe trace:' in s]
        current = stats, trace
        if reference is None:
            reference = current
        else:
            assert current == reference, (command, current, reference)
        records.append(dict(command=command, code=code, expected=expected, stats=stats, output=output))
    print('PASS', model, 'BRAB-2/depth-6 counters and traces in all four modes')
(HERE / '.local/results.json').write_text(json.dumps(records, indent=2) + '\n')
print('PASS', len(records), 'ordinary safe/unsafe integrations')
