import unittest

from analyze_repetitions import analyze_cell


def row(seconds, repetition, status='completed', command=None, stats=None):
    return dict(wall_seconds=seconds, repetition=repetition, status=status,
                command=['cubicle.opt', '-j', '1', 'model.cub'] if command is None else command,
                verdict='SAFE' if status == 'completed' else None,
                stats={'Number of visited nodes': '10'} if stats is None else stats,
                log='run.log', returncode=0 if status == 'completed' else -9)


class RepetitionTests(unittest.TestCase):
    def test_isolated_slow_repetition(self):
        result = analyze_cell([row(.01, 1), row(.011, 2), row(.4, 3)])
        self.assertTrue(result['isolated_slow_candidate'])
        self.assertEqual(result['slow_repetition'], 3)
        self.assertTrue(result['counters_identical'])
        self.assertEqual(result['median_seconds'], .011)

    def test_stable_repetitions(self):
        result = analyze_cell([row(10, 1), row(10.1, 2), row(10.2, 3)])
        self.assertFalse(result['isolated_slow_candidate'])
        self.assertFalse(result['long_run_variability'])

    def test_small_absolute_spread_is_not_flagged_as_isolated_spike(self):
        result = analyze_cell([row(.01, 1), row(.01, 2), row(.04, 3)])
        self.assertGreater(result['max_min_ratio'], 2)
        self.assertFalse(result['isolated_slow_candidate'])

    def test_timeout_is_not_a_runtime_sample(self):
        result = analyze_cell([row(450, 1, status='timeout')])
        self.assertFalse(result['assessable'])
        self.assertEqual(result['times_seconds'], [])
        self.assertIsNone(result['median_seconds'])
        self.assertFalse(result['isolated_slow_candidate'])

    def test_changed_parameters_are_rejected(self):
        with self.assertRaises(ValueError):
            analyze_cell([row(1, 1), row(2, 2, command=['other'])])

    def test_different_search_work_is_reported(self):
        result = analyze_cell([row(1, 1), row(1, 2),
                               row(10, 3, stats={'Number of visited nodes': '20'})])
        self.assertTrue(result['isolated_slow_candidate'])
        self.assertFalse(result['counters_identical'])


if __name__ == '__main__':
    unittest.main()
