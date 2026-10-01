"""Regression tests for the benchmark harness (no Cubicle builds required)."""
import json
from pathlib import Path
import tempfile
import unittest
import bench_compare as bench


class BenchmarkTests(unittest.TestCase):
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
