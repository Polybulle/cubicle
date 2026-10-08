"""Regression tests for the benchmark harness (no Cubicle builds required)."""
import json
from pathlib import Path
import tempfile
import unittest
import bench_compare as bench


class BenchmarkTests(unittest.TestCase):
    def test_flash_forward_recipe(self):
        manifest = json.loads((bench.ROOT / 'experiments/bench_manifest.json').read_text())
        model, = bench.select(manifest, ['flash_nodata_tx.cub'])
        self.assertEqual(bench.configurations(model), ['tetra-fwd', 'old-fwd'])
        self.assertEqual(model['options'], ['-brab', '2'])
        for config in bench.configurations(model):
            command = bench.command_for(model, config,
                                        {'tetra': Path('/tetra'), 'old': Path('/old')},
                                        {model['path']: {'path': '/input'}})
            self.assertEqual(command.count('-tx'), 1)
            self.assertEqual(command[command.index('-tx') + 1], 'fwd')
        source = (bench.ROOT / model['path']).read_text()
        self.assertIn('\ntriggers ni_Wb()\n', source)
        self.assertIn('\ntriggers ni_Replace_shrvld(src) or ni_Replace(src)\n', source)

    def test_cli_accepts_six_workers(self):
        import subprocess
        import sys
        result = subprocess.run([sys.executable, str(Path(bench.__file__)),
                                 '--phase', 'plan', '--jobs', '6'],
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('Workers: 6', result.stdout)

    def test_reuse_requires_matching_inputs_options_budget_and_revision(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / 'old'
            source.mkdir()
            (source / 'logs').mkdir()
            (source / 'logs' / 'run.log').write_text('The system is SAFE')
            model = dict(path='m', group='ordinary', options=[], timeout_seconds=5)
            manifest = {'builds': {n: {'commit': n} for n in ('baseline', 'old', 'tetra')}}
            inputs = {'m': {'sha256': 'same'}}
            bench.save(source / 'manifest.json', manifest)
            bench.save(source / 'inputs.json', inputs)
            row = dict(model='m', config='baseline', phase='measured', repetition=1,
                       command=['binary', *bench.COMMON, 'input'], timeout_seconds=5,
                       status='completed', log='/original/run.log', wall_seconds=1)
            bench.append(source, row)
            for mismatch in ('none', 'input', 'options', 'budget', 'revision'):
                out = root / mismatch
                out.mkdir()
                current = json.loads(json.dumps(manifest))
                candidate = dict(model)
                hashes = inputs
                if mismatch == 'input':
                    hashes = {'m': {'sha256': 'different'}}
                if mismatch == 'options':
                    candidate['options'] = ['-brab']
                if mismatch == 'budget':
                    candidate['timeout_seconds'] = 100
                if mismatch == 'revision':
                    current['builds']['baseline']['commit'] = 'new'
                reused = bench.reuse_results(source, current, [candidate], hashes, out, 1)
                self.assertEqual(reused, {('m', 'baseline')} if mismatch == 'none' else set())
                if reused:
                    stored = json.loads((out / 'runs.jsonl').read_text())
                    self.assertEqual(stored['command'], row['command'])
                    self.assertEqual(stored['reused_from'], str(source))
                    self.assertTrue(Path(stored['log']).is_file())

    def test_six_workers_and_serial_record_writes(self):
        import threading
        from unittest.mock import patch
        barrier = threading.Barrier(6, timeout=5)
        lock = threading.Lock()
        active = peak = 0
        writer_threads = []
        original_append = bench.append

        def invoke(command, cwd, timeout, log, type_only=False):
            nonlocal active, peak
            with lock:
                active += 1
                peak = max(peak, active)
            barrier.wait()
            with lock:
                active -= 1
            return dict(status='completed', verdict='SAFE', timeout_seconds=timeout)

        def append(out, row):
            writer_threads.append(threading.get_ident())
            original_append(out, row)

        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory)
            jobs = [dict(command=['fixture'], timeout_seconds=5, log=str(out / str(i)),
                         model=str(i), config='baseline', phase='measured', repetition=1)
                    for i in range(12)]
            with patch.object(bench, 'invoke', side_effect=invoke), patch.object(bench, 'append', side_effect=append):
                rows = list(bench.execute_jobs(jobs, out, 6, 'Test'))
            stored = [json.loads(line) for line in (out / 'runs.jsonl').read_text().splitlines()]
            self.assertEqual(peak, 6)
            self.assertEqual(len(rows), 12)
            self.assertEqual({r['model'] for r in stored}, {str(i) for i in range(12)})
            self.assertEqual(set(writer_threads), {threading.get_ident()})

    def test_concurrent_real_timeout_does_not_kill_other_runs(self):
        import sys
        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory)
            scripts = ['import time; time.sleep(10)',
                       'import time; time.sleep(.15); print("The system is SAFE")',
                       'print("UNSAFE"); raise SystemExit(1)',
                       'raise SystemExit(2)']
            jobs = [dict(command=[sys.executable, '-c', script],
                         timeout_seconds=.05 if i == 0 else 5,
                         log=str(out / str(i)), model=str(i), config='baseline',
                         phase='pilot', repetition=0) for i, script in enumerate(scripts)]
            rows = {r['model']: r for r in bench.execute_jobs(jobs, out, 4, 'Test')}
            self.assertEqual([rows[str(i)]['status'] for i in range(4)],
                             ['timeout', 'completed', 'completed', 'error'])
            self.assertEqual(rows['1']['verdict'], 'SAFE')
            self.assertEqual(rows['2']['verdict'], 'UNSAFE')
            self.assertTrue(all(Path(r['log']).is_file() for r in rows.values()))

    def test_sequential_cubicle_flag_overrides_model_options(self):
        command = bench.command_for(dict(path='m', options=['-j', '8']), 'baseline',
                                    {'baseline': Path('/build')}, {'m': {'path': '/input'}})
        self.assertEqual(command[-3:], ['-j', '1', '/input'])

    def test_direct_measurement_without_preflight_or_retries(self):
        import random
        from unittest.mock import patch
        model = dict(path='m.cub', group='ordinary', options=[], timeout_seconds=450, expected='SAFE')
        for workers in (1, 4, 6):
            with self.subTest(workers=workers), tempfile.TemporaryDirectory() as directory:
                out = Path(directory)
                dirs = {name: out / name for name in ('baseline', 'tetra', 'old')}
                inputs = {'m.cub': {'path': 'm.cub'}}

                def invoke(command, cwd, timeout, log, type_only=False):
                    self.assertFalse(type_only)
                    self.assertNotIn('-type-only', command)
                    self.assertEqual(timeout, 450)
                    status = ('unsupported' if 'old-all' in log.name else
                              'timeout' if 'tetra-all' in log.name else 'completed')
                    return dict(status=status, verdict='SAFE' if status == 'completed' else None,
                                returncode=0, wall_seconds=.01, timeout_seconds=timeout, log=str(log))

                with patch.object(bench, 'invoke', side_effect=invoke):
                    bench.measure([model], dirs, inputs, out, 3, random.Random(0), workers)
                rows = [json.loads(line) for line in (out / 'runs.jsonl').read_text().splitlines()]
                self.assertEqual([r['phase'] for r in rows], ['measured'] * 11)
                self.assertEqual([r['repetition'] for r in rows], [1] * 5 + [2] * 3 + [3] * 3)
                self.assertEqual({r['config'] for r in rows[:5]}, set(bench.configurations(model)))
                self.assertEqual({r['config'] for r in rows[5:]}, {'baseline', 'tetra-none', 'old-none'})
                self.assertEqual(len({r['log'] for r in rows}), len(rows))
                self.assertFalse((out / 'pilot.json').exists())
                summary = bench.summarize(out, [model])
                self.assertEqual(sum(r['timing_eligible'] for r in summary), 3)
                self.assertEqual(list(bench.execute_jobs([], out, workers, 'Empty')), [])

    def test_later_failure_stops_repetitions_and_excludes_timing(self):
        import random
        from unittest.mock import patch
        model = dict(path='m.cub', group='transaction', options=[], timeout_seconds=5, expected='SAFE')
        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory)

            def invoke(command, cwd, timeout, log, type_only=False):
                status = 'completed' if log.name.endswith('run-1.log') else 'timeout'
                return dict(status=status, verdict='SAFE' if status == 'completed' else None,
                            returncode=0 if status == 'completed' else -9,
                            wall_seconds=.01, timeout_seconds=timeout, log=str(log))

            with patch.object(bench, 'invoke', side_effect=invoke):
                bench.measure([model], {'tetra': out, 'old': out},
                              {'m.cub': {'path': 'm.cub'}}, out, 3, random.Random(0), 4)
            rows = [json.loads(line) for line in (out / 'runs.jsonl').read_text().splitlines()]
            self.assertEqual([r['repetition'] for r in rows], [1, 1, 2, 2])
            summary = bench.summarize(out, [model])
            self.assertFalse(any(r['timing_eligible'] for r in summary))

    def test_snapshot_includes_dirty_and_untracked_inputs(self):
        import subprocess
        from unittest.mock import patch
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'repo'
            root.mkdir()
            subprocess.run(['git', 'init', '-q', str(root)], check=True)
            (root / 'model.cub').write_text('original')
            (root / '.gitignore').write_text('*.opt\n')
            subprocess.run(['git', '-C', str(root), 'add', '.'], check=True)
            subprocess.run(['git', '-C', str(root), '-c', 'user.name=Test',
                            '-c', 'user.email=test@example.invalid', 'commit', '-qm', 'fixture'], check=True)
            (root / 'model.cub').write_text('edited')
            (root / 'new.cub').write_text('new')
            (root / 'cubicle.opt').write_text('ignored')
            out = Path(directory) / 'output'
            out.mkdir()
            with patch.object(bench, 'ROOT', root):
                snapshot = bench.snapshot_working_tree(out)
            self.assertEqual((snapshot / 'model.cub').read_text(), 'edited')
            self.assertEqual((snapshot / 'new.cub').read_text(), 'new')
            self.assertFalse((snapshot / 'cubicle.opt').exists())
            self.assertFalse((snapshot / '.git').exists())
            record = json.loads((out / 'working-tree.json').read_text())
            self.assertIn('model.cub', record['sha256'])
            self.assertIn('new.cub', record['sha256'])

    def test_progress_bar(self):
        import io
        from contextlib import redirect_stdout
        output = io.StringIO()
        with redirect_stdout(output):
            bench.progress('Runs', 1, 2, 'bakery.cub tetra-all')
            bench.progress('Runs', 2, 2)
            bench.progress('Runs', 0, 0)
        text = output.getvalue()
        self.assertIn('[##########----------] 1/2', text)
        self.assertIn('bakery.cub tetra-all', text)
        self.assertIn('[####################] 2/2', text)
        self.assertIn('0/0', text)

    def test_terminal_progress_updates_in_place(self):
        import io
        from unittest.mock import patch
        output = io.StringIO()
        with patch.object(bench.sys, 'stdout', output), patch.object(output, 'isatty', return_value=True):
            bench.progress('Runs', 0, 1, 'current job')
            self.assertFalse(output.getvalue().endswith('\n'))
            bench.progress('Runs', 1, 1)
        self.assertEqual(output.getvalue().count('\r\033[2K'), 2)
        self.assertTrue(output.getvalue().endswith('\n'))

    def test_timing_uses_blocking_wait_without_polling_delay(self):
        from unittest.mock import patch
        real_popen = bench.subprocess.Popen

        def checked_popen(*args, **kwargs):
            proc = real_popen(*args, **kwargs)
            real_wait = proc.wait

            def checked_wait(*args, **kwargs):
                self.assertFalse(args or kwargs, 'Timed wait polling biases short benchmarks')
                return real_wait()

            proc.wait = checked_wait
            return proc

        with tempfile.TemporaryDirectory() as directory:
            with patch.object(bench.subprocess, 'Popen', checked_popen):
                row = bench.invoke(['/usr/bin/true'], Path(directory), 10, Path(directory) / 'log')
            self.assertEqual(row['returncode'], 0)

    def test_same_num_include_for_every_revision(self):
        command = bench.make_command('/test/num', 'abcdef')
        self.assertIn('INCLUDES=$(INCLPATHS) $(Z3CCFLAGS) -I /test/num -I +unix', command)
        self.assertIn('VERSION_STR=abcdef', command)

    def test_unsafe_exit_one_is_a_result(self):
        self.assertEqual(bench.classify('UNSAFE\nCounterexample', 1), ('completed', 'UNSAFE'))

    def test_unsafe_requires_exit_one(self):
        for code in (0, 2, 127):
            with self.subTest(code=code):
                self.assertEqual(bench.classify('UNSAFE\n', code), ('error', None))

    def test_exit_one_without_verdict_is_not_unsafe(self):
        self.assertEqual(bench.classify('Internal failure: failed', 1), ('error', None))

    def test_commands_preserve_statistics_output(self):
        self.assertNotIn('-quiet', bench.COMMON)

    def test_timeout_is_not_safe(self):
        self.assertEqual(bench.classify('The system is SAFE', -9, True), ('timeout', None))

    def test_crash_is_not_a_verdict(self):
        self.assertEqual(bench.classify('The system is SAFE', -11), ('crash', None))

    def test_failed_safe_command_is_not_completed(self):
        self.assertEqual(bench.classify('The system is SAFE', 1), ('error', None))

    def test_cycle_rejection_is_unsupported(self):
        self.assertEqual(bench.classify('Found a cycle of triggers (forbidden)', 1), ('unsupported', None))

    def test_conflicting_verdicts_are_not_completed(self):
        self.assertEqual(bench.classify('The system is SAFE\nUNSAFE', 0), ('conflicting-output', None))

    def test_transaction_group_only_has_all_builds(self):
        self.assertEqual(bench.configurations({'group': 'transaction'}), ['tetra-all', 'old-all'])

    def test_model_can_select_only_its_requested_configuration(self):
        self.assertEqual(bench.configurations({'group': 'ordinary', 'configs': ['tetra-all']}),
                         ['tetra-all'])
        self.assertEqual(bench.configurations({'group': 'ordinary', 'configs': ['tetra-none']}),
                         ['tetra-none'])

    def test_configuration_subset_rejects_invalid_or_duplicate_entries(self):
        for configs in ([], ['missing'], ['tetra-all', 'tetra-all']):
            with self.subTest(configs=configs), self.assertRaises(ValueError):
                bench.configurations({'group': 'ordinary', 'configs': configs})
        with self.assertRaises(ValueError):
            bench.configurations({'group': 'transaction', 'configs': ['baseline']})

    def test_unknown_model_is_rejected(self):
        with self.assertRaises(ValueError):
            bench.select({'models': []}, ['missing.cub'])

    def test_timeout_kills_real_process(self):
        import sys
        with tempfile.TemporaryDirectory() as directory:
            result = bench.invoke([sys.executable, '-c', 'import time; time.sleep(10)'],
                                  Path(directory), 0.05, Path(directory) / 'log')
            self.assertEqual(result['status'], 'timeout')
            self.assertLess(result['wall_seconds'], 2)

    def test_disagreement_excludes_timing(self):
        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory)
            rows = [dict(model='m.cub', config='tetra-all', phase='measured', repetition=1,
                         status='completed', verdict='SAFE', returncode=0, wall_seconds=1,
                         timeout_seconds=30, log='safe'),
                    dict(model='m.cub', config='old-all', phase='measured', repetition=1,
                         status='completed', verdict='UNSAFE', returncode=1, wall_seconds=1,
                         timeout_seconds=30, log='unsafe')]
            (out / 'runs.jsonl').write_text(''.join(json.dumps(r) + '\n' for r in rows))
            result = bench.summarize(out, [{'path': 'm.cub', 'group': 'transaction', 'expected': None}])
            self.assertTrue(all(r['verdict_disagreement'] for r in result))
            self.assertFalse(any(r['timing_eligible'] for r in result))

    def test_summary_preserves_preflight_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory)
            row = dict(model='m.cub', config='old-all', phase='type', repetition=0,
                       status='unsupported', verdict=None, returncode=2, wall_seconds=0.01,
                       timeout_seconds=30, log='rejected')
            (out / 'runs.jsonl').write_text(json.dumps(row) + '\n')
            result = bench.summarize(out, [{'path': 'm.cub', 'group': 'transaction', 'expected': None}])
            old = next(r for r in result if r['config'] == 'old-all')
            self.assertEqual(old['statuses'], ['unsupported'])


if __name__ == '__main__':
    unittest.main()
