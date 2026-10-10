#!/usr/bin/env python3
"""Independent finite host-step replay; NOT an unbounded proof or general parser.

Checks native model variants have exactly the declared executable differences,
then executes hand-selected adversarial schedules with real local guards and
simultaneous committed updates. The replay retains all historical envelopes.
"""
from dataclasses import dataclass, field
import json
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent


def executable(path):
    return re.sub(r'\(\*.*?\*\)', '', path.read_text(), flags=re.S)


@dataclass
class Model:
    mutant: int = 0
    held: dict = field(default_factory=lambda: {'A': True, 'B': False})
    data: dict = field(default_factory=lambda: {'A': 0, 'B': 0})
    expected: int = 0
    seq: dict = field(default_factory=dict)
    recv: dict = field(default_factory=dict)
    records: dict = field(default_factory=dict)
    inbox: dict = field(default_factory=dict)
    wire: dict = field(default_factory=dict)
    trace: list = field(default_factory=list)
    sends: dict = field(default_factory=lambda: {'A': 0, 'B': 0})

    def note(self, step):
        self.trace.append({'step': step, 'held': dict(self.held),
                           'inbox': dict(self.inbox), 'expected': self.expected,
                           'violations': self.violations()})

    def violations(self):
        claims = [('host', h) for h, own in self.held.items() if own]
        claims += [('inbox', h) for h in self.inbox]
        claims += [('flight', m) for m, r in self.records.items()
                   if r['published'] and r['n'] > self.recv.get((r['d'], r['s']), 0)]
        bad = []
        if len(claims) != 1:
            bad.append('claim conservation: ' + repr(claims))
        if any(self.data[h] != self.expected for h, own in self.held.items() if own):
            bad.append('authoritative data differs from Expected')
        if any(self.records[m]['value'] != self.expected for m in self.inbox.values()):
            bad.append('buffered data differs from Expected')
        if any(r['buffers'] > 1 or r['installs'] > 1 for r in self.records.values()):
            bad.append('envelope dispatched more than once')
        return bad

    def send(self, s, d, m, publish=True):
        assert self.held[s] and s != d and m not in self.records and m not in self.held
        snapshot = self.data[s]
        n = self.seq.get((s, d), 0) + 1
        self.seq[s, d] = n
        self.held[s] = False
        self.sends[s] += 1
        self.records[m] = dict(s=s, d=d, n=n, value=snapshot, pending=publish,
                               ack=False, buffers=0, installs=0, published=publish)
        tail = '/publish' if publish else '/INTERNAL-before-publish'
        self.note(f'send/snapshot/relinquish{tail}({s},{d},{m})')

    def publish(self, m):
        assert m in self.records and not self.records[m]['published']
        self.records[m]['published'] = True
        self.records[m]['pending'] = True
        self.note(f'publish({m})')

    def deliver(self, m):
        r = self.records[m]
        assert r['published'] and r['d'] not in self.wire
        self.wire[r['d']] = m
        self.note(f'deliver({m})')

    def receive(self, m):
        r = self.records[m]
        s, d, n = r['s'], r['d'], r['n']
        assert self.wire[d] == m
        hi = self.recv.get((d, s), 0)
        if self.mutant == 1 or n == hi + 1:
            assert d not in self.inbox
            self.recv[d, s] = n
            self.inbox[d] = m
            r['buffers'] += 1
            r['ack'] = True
            branch = 'new/buffer/ack'
        elif n <= hi:
            r['ack'] = True
            branch = 'duplicate/ack'
        else:
            branch = 'gap/reject'
        del self.wire[d]
        self.note(f'receive-{branch}({m})')

    def process(self, d):
        assert d in self.inbox
        m = self.inbox[d]
        r = self.records[m]
        self.held[d] = True
        self.data[d] = r['value']
        r['installs'] += 1
        if self.mutant != 3:
            del self.inbox[d]
        self.note(f'process/install/clear({d},{m})')

    def cleanup(self, m):
        r = self.records[m]
        s, d, n = r['s'], r['d'], r['n']
        assert r['ack'] and n <= self.seq[s, d]
        for other in self.records.values():
            if other['s'] == s and other['d'] == d and other['n'] <= n:
                other['pending'] = False
        if self.mutant == 2:
            self.recv[d, s] = 0
        self.note(f'cleanup({m})')

    def set_one(self, h):
        assert self.held[h]
        self.data[h] = self.expected = 1
        self.note(f'SET({h},One)')

    def handoff(self, s, d, m, early_ack=False):
        self.send(s, d, m)
        self.deliver(m)
        self.receive(m)
        if early_ack:
            self.cleanup(m)
            assert d in self.inbox and not self.records[m]['pending']
        self.process(d)


def replay_native(rows):
    """Decode the supplied single-envelope shortest mutant traces only."""
    checks = {}
    names = {1: 'm1-no-dedup.cub', 2: 'm2-reset-watermark.cub', 3: 'm3-retain-inbox.cub'}
    for number, name in names.items():
        matches = [r for r in rows if r['file'] == name and r['verdict'] == 'UNSAFE']
        if not matches:
            continue
        row = min(matches, key=lambda r: r['seconds'])
        text = Path(row['log']).read_text()
        trace = re.search(r'Unsafe trace:(.*?)(?=={10,})', text, re.S).group(1)
        calls = re.findall(r'(\w+)\((#[\d]+(?:, #[\d]+)*)\)', trace)
        args = next(a.split(', ') for t, a in calls if t == 'send')
        mapping = {args[0]: 'A', args[1]: 'B', args[2]: 'm1'}
        model = Model(mutant=number)
        active = {}
        for transition, raw in calls:
            a = [mapping[v] for v in raw.split(', ')]
            if transition == 'provision':
                assert a == ['A']
            elif transition == 'send':
                active['send'] = a
            elif transition == 'relinquish':
                assert active['send'] == a
            elif transition == 'publish':
                assert active.pop('send') == a
                model.send(*a)
            elif transition == 'deliver':
                model.deliver(a[2])
            elif transition == 'receive_new':
                active['receive'] = a
            elif transition == 'buffer':
                assert active['receive'] == a
            elif transition == 'emit_ack':
                assert active.pop('receive') == a
                model.receive(a[2])
            elif transition == 'process_buffered':
                active['install'] = a
            elif transition == 'clear_inbox':
                assert active.pop('install') == a
                model.process(a[0])
            elif transition == 'cleanup':
                model.cleanup(a[2])
            else:
                raise AssertionError(transition)
        assert not active and model.violations()
        checks[name] = dict(log=row['log'], all_handlers_completed=True,
                            decoded_trace=model.trace)
    return checks


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--native-results', type=Path,
                        help='JSONL containing supplied single-envelope mutant traces')
    args = parser.parse_args()
    original = executable(HERE / 'ironkv-migration.cub')
    replacements = {
        1: ('Number[m] = Recv[d,s] + 1 }', 'Number[m] > 0 }'),
        2: ('{ Pending[q] := case', '{ Recv[d,s] := 0; Pending[q] := case'),
        3: ('Inbox[d] := False }', 'Inbox[d] := True }'),
    }
    names = {1: 'm1-no-dedup.cub', 2: 'm2-reset-watermark.cub', 3: 'm3-retain-inbox.cub'}
    for variant, (old, new) in replacements.items():
        assert original.count(old) == 1
        assert executable(HERE / names[variant]) == original.replace(old, new)
    strip_queries = lambda s: re.sub(r'unsafe\s*\([^)]*\)\s*\{[^}]*\}\s*', '', s)
    normalize = lambda s: re.sub(r'\s+', '', strip_queries(s))
    assert normalize(original) == normalize(executable(HERE / 'completion-witness.cub'))
    # No ghost ownership witness is read in an operational guard.
    for guard in re.findall(r'requires\s*\{([^}]+)\}', original):
        assert not re.search(r'\b(Claim|Witness|ClaimPacket|Expected|Buffers|Installs)\b', guard)

    safe = Model()
    safe.handoff('A', 'B', 'm1', early_ack=True)
    safe.set_one('B')
    safe.handoff('B', 'A', 'm2')
    safe.deliver('m1'); safe.receive('m1')  # stale payload after SET and return
    safe.handoff('A', 'B', 'm3')
    safe.cleanup('m1')  # stale ACK must preserve newer m3
    assert safe.records['m3']['pending']
    safe.handoff('B', 'A', 'm4')
    assert safe.sends['A'] == 2 and safe.held['A']
    assert all(not row['violations'] for row in safe.trace)
    records = {'safe-repeated-with-early-ack-stale-data-and-stale-ack': safe.trace}
    partial = Model()
    partial.send('A', 'B', 'm1', publish=False)
    assert partial.violations() and not any(partial.held.values())
    partial.publish('m1')
    assert not partial.violations()
    records['ignore-internal-no-claim-before-publication'] = partial.trace
    for variant in (1, 2, 3):
        model = Model(mutant=variant)
        if variant == 3:
            model.handoff('A', 'B', 'm1')
        else:
            model.handoff('A', 'B', 'm1')
            model.set_one('B')
            model.handoff('B', 'A', 'm2')
            if variant == 2:
                model.cleanup('m1')
            model.deliver('m1'); model.receive('m1'); model.process('B')
        assert model.violations(), f'mutant {variant} not detected'
        records[f'm{variant}-completed-boundary-counterexample'] = model.trace
    output = HERE / '.local' / 'concrete-replays.json'
    output.parent.mkdir(exist_ok=True)
    output.write_text(json.dumps(records, indent=2) + '\n')
    native = {}
    if args.native_results:
        rows = [json.loads(line) for line in args.native_results.read_text().splitlines()]
        native = replay_native(rows)
        (HERE / '.local' / 'native-counterexample-replays.json').write_text(
            json.dumps(native, indent=2) + '\n')
    print(json.dumps({'replays': len(records), 'safe_trace_steps': len(safe.trace),
                      'native_completed_traces': len(native),
                      'checks': 'guards, claims, values, repeated hosts, early ACK, stale ACK, variant diffs',
                      'output': str(output)}))


if __name__ == '__main__':
    main()
