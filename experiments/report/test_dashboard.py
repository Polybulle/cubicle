import json
import os
import copy
from pathlib import Path
import statistics
import unittest

from build_dashboard import load_data, render
import build_dashboard

ROOT = Path(os.environ.get('CUBICLE_BENCH_ARCHIVE', Path(__file__).parent))

@unittest.skipUnless((ROOT / 'run-08' / 'summary.json').is_file(),
                     'Set CUBICLE_BENCH_ARCHIVE to the retained benchmark archive')
class DashboardTests(unittest.TestCase):
    def test_sequential_run_includes_forward_only_cells(self):
        data = load_data(ROOT / 'run-08')
        self.assertEqual(data['environment']['workers'], 1)
        self.assertEqual(len(data['runs']), 370)
        flash = [s for s in data['summary'] if s['model'] == 'examples/flash_nodata_tx.cub']
        self.assertEqual({s['config'] for s in flash}, {'tetra-fwd', 'old-fwd'})
        self.assertTrue(all(s['timing_eligible'] for s in flash))
        self.assertEqual(data['repetitions_url'], 'run-08/outliers.html')
        for summary in data['summary']:
            if summary['timing_eligible']:
                rows = [r for r in data['runs'] if (r['model'], r['config']) ==
                        (summary['model'], summary['config'])]
                self.assertEqual(summary['median_seconds'], statistics.median(r['wall_seconds'] for r in rows))

    def test_multiple_overlays_keep_original_view_and_each_source(self):
        base = load_data(ROOT / 'run-05')
        data = build_dashboard.integrate_updates(base, load_data(ROOT / 'run-06'))
        data = build_dashboard.integrate_updates(data, load_data(ROOT / 'run-07'))
        self.assertEqual(data['original']['summary'], base['summary'])
        self.assertEqual(data['original']['runs'], base['runs'])
        self.assertEqual([s['name'] for s in data['overlays']], ['run-06', 'run-07'])
        self.assertEqual(len(data['summary']), 128)
        self.assertEqual(len(data['runs']), 366)
        self.assertEqual(sum(s.get('updated_from') == 'run-06' for s in data['summary']), 4)
        self.assertEqual(sum(s.get('updated_from') == 'run-07' for s in data['summary']), 3)
        for source in ('run-06', 'run-07'):
            expected = {(s['model'], s['config']): s for s in load_data(ROOT / source)['summary']}
            for entry in data['summary']:
                if entry.get('updated_from') == source:
                    self.assertEqual(entry['median_seconds'], expected[entry['model'], entry['config']]['median_seconds'])

    def test_focused_overlay_replaces_only_four_cells_without_mutating_archives(self):
        base = load_data(ROOT / 'run-05')
        update = load_data(ROOT / 'run-06')
        saved = copy.deepcopy(base)
        data = build_dashboard.integrate_updates(base, update)
        self.assertEqual(base, saved)
        self.assertEqual(len(data['summary']), 128)
        self.assertEqual(len(data['runs']), 366)
        self.assertEqual(data['original']['summary'], base['summary'])
        self.assertEqual(data['original']['runs'], base['runs'])
        changed = {(s['model'], s['config']): s for s in update['summary']}
        self.assertEqual(len(changed), 4)
        for entry in data['summary']:
            key = entry['model'], entry['config']
            if key in changed:
                self.assertEqual(entry['median_seconds'], changed[key]['median_seconds'])
                self.assertEqual(entry['updated_from'], 'run-06')
            else:
                self.assertEqual(entry, next(s for s in base['summary'] if (s['model'], s['config']) == key))
        for row in data['runs']:
            if (row['model'], row['config']) in changed:
                self.assertTrue(row['log_url'].startswith('run-06/'))
                self.assertEqual(row['updated_from'], 'run-06')

    def test_overlay_rejects_different_inputs_or_options(self):
        base = load_data(ROOT / 'run-05')
        for field in ('input_hashes', 'options'):
            update = load_data(ROOT / 'run-06')
            path = update['models'][0]['path']
            if field == 'input_hashes':
                update['input_hashes'][path] = 'different'
            else:
                update['models'][0]['options'] = ['-nodes', '1']
            with self.subTest(field=field), self.assertRaises(ValueError):
                build_dashboard.integrate_updates(base, update)

    def test_overlay_rechecks_verdict_disagreement_across_all_configurations(self):
        base = load_data(ROOT / 'run-05')
        update = load_data(ROOT / 'run-06')
        path = 'examples/chandra_toueg.cub'
        for data in (base, update):
            next(m for m in data['models'] if m['path'] == path)['expected'] = 'SAFE'
        for row in update['runs']:
            if row['model'] == path:
                row['verdict'] = 'UNSAFE'
                row['returncode'] = 1
        data = build_dashboard.integrate_updates(base, update)
        for entry in data['summary']:
            if entry['model'] == path:
                self.assertTrue(entry['verdict_disagreement'])
                self.assertTrue(entry['expectation_mismatch'])
                self.assertFalse(entry['timing_eligible'])

    def test_overlay_rejects_duplicate_or_unknown_cells(self):
        base = load_data(ROOT / 'run-05')
        for invalid in ('duplicate', 'unknown'):
            update = load_data(ROOT / 'run-06')
            if invalid == 'duplicate':
                update['summary'].append(copy.deepcopy(update['summary'][0]))
            else:
                update['summary'][0]['config'] = 'missing'
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                build_dashboard.integrate_updates(base, update)

    def test_complete_source_and_measured_medians(self):
        data = load_data(ROOT / 'run-01')
        self.assertEqual(len(data['models']), 28)
        self.assertEqual(len(data['summary']), 128)
        self.assertEqual(len(data['runs']), 543)
        self.assertEqual(sum(r['phase'] == 'measured' for r in data['runs']), 291)
        for s in data['summary']:
            runs = [r for r in data['runs'] if r['phase'] == 'measured' and r['model'] == s['model'] and r['config'] == s['config']]
            self.assertEqual(len(runs), s['measured_runs'])
            if s['timing_eligible']:
                self.assertAlmostEqual(statistics.median(r['wall_seconds'] for r in runs), s['median_seconds'])
            else:
                self.assertIsNone(s['median_seconds'])

    def test_local_links_and_embedded_data(self):
        data = load_data(ROOT / 'run-01')
        for r in data['runs']:
            if r['log_url']:
                self.assertTrue((ROOT / r['log_url']).is_file())
        html = render(data)
        self.assertIn('<title>Cubicle · Benchmark explorer</title>', html)
        self.assertNotIn('https://cdn', html)
        payload = html.split('<script id="data" type="application/json">')[1].split('</script>')[0]
        self.assertEqual(json.loads(payload), data)

    def test_script_injection_escaped(self):
        html = render({'test': '</script><script>alert(1)</script>'})
        self.assertNotIn('</script><script>alert', html)

if __name__ == '__main__':
    unittest.main()
