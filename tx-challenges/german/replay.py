"""Independent finite concrete evaluator for this family's actual .cub text.
Not an unbounded checker. Assert guards, simultaneous updates, CFG successor
calls for snapshot steps, coherence and bulk/incremental boundary equality.
"""
import copy
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent

class Model:
    def __init__(self, name, n=3):
        self.name, self.n = name, n
        text = re.sub(r'\(\*.*?\*\)', '', (ROOT/name).read_text(), flags=re.S)
        self.const = {}
        for rhs in re.findall(r'type \w+\s*=([^\n]+)', text):
            self.const.update({v.strip(): v.strip() for v in rhs.split('|')})
        self.const.update(True_=True, False_=False)
        self.state = {}
        for key in re.findall(r'var (\w+)\s*:', text):
            self.state[key] = None
        for key in re.findall(r'array (\w+)\[proc\]', text):
            self.state[key] = [None]*n
        init = re.search(r'init \(z\)\s*\{(.*?)\}', text, re.S)[1]
        for atom in init.split('&&'):
            lhs, rhs = atom.strip().split(' = ')
            val = self.expr(rhs.strip(), {})
            if '[' in lhs:
                self.state[lhs.split('[')[0]] = [val]*n
            else:
                self.state[lhs] = val
        self.transitions = {}
        for m in re.finditer(r'(triggered\s+)?transition (\w+)\(x\)\s*requires\s*\{(.*?)\}\s*\{(.*?)\}(?:\s*triggers ([^\n]+))?', text, re.S):
            internal, name, guard, actions, calls = m.groups()
            self.transitions[name] = (bool(internal), guard.strip(), actions, re.findall(r'(\w+)\(', calls or ''))
        self.next = None
        self.trace = []

    def expr(self, expr, bindings):
        expr = ' '.join(expr.split()).replace('&&',' and ').replace('<>','!=')
        expr = re.sub(r'(?<![<>=!])=(?!=)', '==', expr)
        expr = re.sub(r'\bTrue\b','True_',expr)
        expr = re.sub(r'\bFalse\b','False_',expr)
        env = dict(self.const, **self.state, **bindings)
        return eval(expr, {'__builtins__': {}}, env)

    def guard(self, guard, x):
        if 'forall_other' in guard:
            local, quant = guard.split('forall_other',1)
            local = re.sub(r'&&\s*$', '',local).strip()
            var, body = quant.strip().split('.',1)
            return self.expr(local, {'x':x}) and all(self.expr(body, {'x':x,var.strip():j}) for j in range(self.n) if j != x)
        return self.expr(guard, {'x':x})

    def step(self, name, x):
        internal, guard, actions, calls = self.transitions[name]
        if self.next is not None:
            assert name in self.next, (name, self.next)
        else:
            assert not internal, name
        assert self.guard(guard,x), (self.name,name,x,self.state)
        new = copy.deepcopy(self.state)
        for action in actions.split(';'):
            if not action.strip():
                continue
            lhs, rhs = [s.strip() for s in action.split(':=',1)]
            key = lhs.split('[')[0]
            for j in range(self.n) if '[' in lhs else [None]:
                bindings = {'x':x, 'j':j}
                if rhs.startswith('case'):
                    val = None
                    for branch in rhs[4:].split('|'):
                        if not branch.strip():
                            continue
                        cond, value = branch.split(':',1)
                        if cond.strip() == '_' or self.expr(cond,bindings):
                            val = self.expr(value,bindings)
                            break
                else:
                    val = self.expr(rhs, bindings)
                if j is None:
                    new[key] = val
                else:
                    new[key][j] = val
        self.state = new
        self.next = calls or None
        self.trace.append(dict(step=f'{name}({x})', state=copy.deepcopy(new), committed=self.next is None))

    def coherent(self):
        return all(a != 'Exclusive' or all(b == 'Invalid' for j,b in enumerate(self.state['Cache']) if j != i) for i,a in enumerate(self.state['Cache']))


def witness(name, order=(0,1), mutant=False):
    m = Model(name)
    bulk = name == 'german-bulk.cub'
    def step(t,x):
        m.step(t,x)
        if not mutant:
            assert m.coherent(), m.trace[-1]
    def snapshot(t,x,members):
        step(t,x)
        if not bulk:
            for p in members:
                step('copy',p)
            step('finish',0)
    step('t10',0); snapshot('t3',0,[]); step('t1',0); step('t13',0)
    step('t10',1); snapshot('t3',1,[0]); step('t1',1); step('t13',1)
    step('t11',2); snapshot('t3bis',2,order)
    step('t8',0); step('t8',1)
    if not mutant:
        step('t12',0); step('t9',0); step('t12',1); step('t9',1)
    step('t2',2); step('t14',2)
    assert (not m.coherent()) if mutant else m.coherent()
    return m

if __name__ == '__main__':
    baseline = witness('german-bulk.cub')
    records = []
    for name in ['german-bulk.cub','german-incremental.cub','german-incremental-tx.cub']:
        for order in [(0,1),(1,0)]:
            m = witness(name,order)
            assert m.state == baseline.state
            records.append(dict(model=name, order=order, result='completed coherent grant; final state equals bulk',trace=m.trace))
    mutant = witness('german-premature-grant-mutant.cub',mutant=True)
    records.append(dict(model=mutant.name,result='completed snapshot then premature grant violates coherence',trace=mutant.trace))
    output = ROOT / '.local'
    output.mkdir(exist_ok=True)
    (output/'concrete-traces.json').write_text(json.dumps(records,indent=2)+'\n')
    print('PASS: 6 coherent executions, both two-participant copy orders; bulk final-state equality; 1 concrete faulty committed grant.')
