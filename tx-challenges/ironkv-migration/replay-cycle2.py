#!/usr/bin/env python3
"""Finite M4/M5 schedules and executable-diff audit; not a safety proof."""
import json
from pathlib import Path
import re
from replay import Model, executable

HERE = Path(__file__).resolve().parent


class DeliveryModel(Model):
    def cleanup(self, m):
        r = self.records[m]
        s, d, n = r['s'], r['d'], r['n']
        assert r['ack'] and n <= self.seq[s, d]
        for other in self.records.values():
            same_pair = other['s'] == s and other['d'] == d
            if (same_pair or self.mutant == 5) and other['n'] <= n:
                other['pending'] = False
        if self.mutant == 4:
            self.seq[s, d] = 0
        self.note(f'cleanup({m})')

    def violations(self):
        bad = super().violations()
        for m, r in self.records.items():
            if (r['published'] and r['n'] > self.recv.get((r['d'], r['s']), 0)
                    and not r['pending']):
                bad.append('unreceived claim missing Pending: ' + m)
        return bad


def replay_native(rows):
    checks = {}
    for variant, name in [(4, 'm4-sequence-reuse.cub'), (5, 'm5-wrong-endpoint-ack.cub')]:
        matches = [row for row in rows if Path(row['file']).name == name
                   and row['verdict'] == 'UNSAFE']
        if not matches:
            continue
        row = min(matches, key=lambda item: item['seconds'])
        text = Path(row['log']).read_text()
        match = re.search(r'Unsafe trace:(.*?)(?=={10,})', text, re.S)
        assert match is not None
        calls = [(t, a.split(', ')) for t, a in
                 re.findall(r'(\w+)\((#[\d]+(?:, #[\d]+)*)\)', match.group(1))]
        hosts = set()
        packets = set()
        for transition, args in calls:
            if transition == 'send':
                hosts.update(args[:2]); packets.add(args[2])
            elif transition == 'provision':
                hosts.add(args[0])
        assert not hosts.intersection(packets)
        provision = next(args[0] for t, args in calls if t == 'provision')
        mapping = {h: 'H' + h[1:] for h in hosts}
        mapping.update({m: 'm' + m[1:] for m in packets})
        model = DeliveryModel(mutant=variant,
                              held={mapping[h]: h == provision for h in hosts},
                              data={mapping[h]: 0 for h in hosts},
                              sends={mapping[h]: 0 for h in hosts})
        active = {}
        for transition, raw in calls:
            a = [mapping[v] for v in raw]
            if transition == 'provision':
                assert raw == [provision] and not model.trace
            elif transition == 'send':
                assert not active
                active['send'] = a
            elif transition == 'relinquish':
                assert active['send'] == a
            elif transition == 'publish':
                assert active.pop('send') == a
                model.send(*a)
            elif transition == 'deliver':
                model.deliver(a[2])
            elif transition == 'receive_new':
                r = model.records[a[2]]
                assert r['n'] == model.recv.get((a[1], a[0]), 0) + 1
                active['receive'] = a
            elif transition == 'buffer':
                assert active['receive'] == a
            elif transition == 'emit_ack':
                assert active.pop('receive') == a
                model.receive(a[2])
            elif transition == 'receive_duplicate':
                r = model.records[a[2]]
                assert r['n'] <= model.recv.get((a[1], a[0]), 0)
                model.receive(a[2])
            elif transition == 'reject_gap':
                r = model.records[a[2]]
                assert r['n'] > model.recv.get((a[1], a[0]), 0) + 1
                model.receive(a[2])
            elif transition == 'process_buffered':
                active['install'] = a
            elif transition == 'clear_inbox':
                assert active.pop('install') == a
                model.process(a[0])
            elif transition == 'cleanup':
                assert not active
                model.cleanup(a[2])
            elif transition in ('set_zero', 'set_one'):
                assert model.held[a[0]] and not active
                model.data[a[0]] = model.expected = int(transition == 'set_one')
                model.note(transition + '(' + a[0] + ')')
            elif transition == 'drop_wire':
                assert a[0] in model.wire
                del model.wire[a[0]]
                model.note('drop_wire(' + a[0] + ')')
            elif transition == 'retransmit':
                assert model.records[a[2]]['pending'] and not active
                model.note('retransmit(' + a[2] + ')')
            else:
                raise AssertionError(transition)
        assert not active and model.violations()
        checks[name] = dict(log=row['log'], all_handlers_completed=True,
                            mapping=mapping, decoded_trace=model.trace)
    return checks


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--native-results', type=Path)
    args = parser.parse_args()
    original = executable(HERE / 'ironkv-migration.cub')
    normalize = lambda text: re.sub(r'\s+', '', text)
    replacements = {
        'm4-sequence-reuse.cub': ('{ Pending[q] := case',
                                '{ Seq[s,d] := 0; Pending[q] := case'),
        'm5-wrong-endpoint-ack.cub': (
            'Src[q] = s && Dst[q] = d && Number[q] <= Number[m] : False',
            'Number[q] <= Number[m] : False'),
    }
    for name, (old, new) in replacements.items():
        assert original.count(old) == 1
        assert normalize(executable(HERE / name)) == normalize(original.replace(old, new))
    strip_queries = lambda text: re.sub(r'unsafe\s*\([^)]*\)\s*\{[^}]*\}', '', text)
    slices = sorted((HERE / '.local/slices').glob('[a-f]-*.cub'))
    assert len(slices) == 6
    for path in slices:
        assert normalize(strip_queries(executable(path))) == normalize(strip_queries(original))
    for guard in re.findall(r'requires\s*\{([^}]+)\}', original):
        assert not re.search(r'\b(Claim|Witness|ClaimPacket|Expected|Buffers|Installs)\b', guard)

    traces = {}
    for variant in (0, 4):
        model = DeliveryModel(mutant=variant)
        model.handoff('A', 'B', 'm1', early_ack=True)
        model.handoff('B', 'A', 'm2')
        assert all(not row['violations'] for row in model.trace)
        model.send('A', 'B', 'm3')
        if variant == 4:
            assert model.records['m3']['n'] == model.records['m1']['n'] == 1
            assert model.violations()
        else:
            assert model.records['m3']['n'] == 2 and not model.violations()
        model.deliver('m3'); model.receive('m3')
        if variant == 4:
            assert not model.inbox and not any(model.held.values())
            assert model.records['m3']['buffers'] == 0
        else:
            assert model.inbox['B'] == 'm3' and not model.violations()
        traces['m4' if variant else 'm4-safe-control'] = model.trace
    for variant in (0, 5):
        model = DeliveryModel(mutant=variant,
                              held={'A': True, 'B': False, 'C': False},
                              data={'A': 0, 'B': 0, 'C': 0},
                              sends={'A': 0, 'B': 0, 'C': 0})
        model.handoff('A', 'B', 'm1')
        model.handoff('B', 'A', 'm2')
        model.send('A', 'C', 'm3')
        assert all(not row['violations'] for row in model.trace)
        model.cleanup('m1')
        assert model.records['m3']['pending'] == (variant == 0)
        assert bool(model.violations()) == (variant == 5)
        traces['m5' if variant else 'm5-safe-control'] = model.trace
    output = HERE / '.local/cycle2-concrete-replays.json'
    output.write_text(json.dumps(traces, indent=2) + '\n')
    native = {}
    if args.native_results:
        rows = [json.loads(line) for line in args.native_results.read_text().splitlines()]
        native = replay_native(rows)
        (HERE / '.local/cycle2-native-replays.json').write_text(
            json.dumps(native, indent=2) + '\n')
    print(json.dumps({'slices_transition_identical': len(slices),
                      'mutant_single_changes': list(replacements),
                      'finite_traces': len(traces), 'native_completed_traces': len(native),
                      'output': str(output)}))


if __name__ == '__main__':
    main()
