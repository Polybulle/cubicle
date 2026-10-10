"""Concrete execution of generated .cub guards, simultaneous updates and calls.
Finite evidence only; this does not replace an unbounded checker result.
"""
from pathlib import Path
import re, itertools, json
HERE=Path(__file__).resolve().parent
DOMAIN=tuple(range(6))
CONSTS={x:x for x in 'I S E Empty Get GetX Put PutX Nak NoInv Inv Ack Idle Accepted Working Received'.split()}
CONSTS.update({'True':True,'False':False})
def expr(text,state,bind):
    env={**CONSTS,**state,**bind}
    text=text.strip().replace('&&',' and ').replace('||',' or ').replace('<>','!=')
    text=re.sub(r'(?<![!<>=])=(?!=)','==',text)
    text=re.sub(r'\b([A-Za-z][A-Za-z0-9_]*)\b',lambda m:'env['+repr(m[1])+']' if m[1] not in ('and','or','not') else m[1],text)
    return eval(text,{'__builtins__':{}},{'env':env})
def guard(text,state,bind):
    if 'forall_other' in text:
        prefix,univ=text.split('forall_other')
        name,body=univ.split('.',1)
        prefix=prefix.strip().removesuffix('&&').strip()
        return (not prefix or expr(prefix,state,bind)) and all(expr(body,state,{**bind,name.strip():v}) for v in DOMAIN if v not in bind.values())
    return expr(text,state,bind)
def parse(path):
    text=re.sub(r'\(\*.*?\*\)','',path.read_text(),flags=re.S)
    transitions={}
    for m in re.finditer(r'(triggered\s+)?transition\s+(\w+)\s*\(([^)]*)\)\s*requires\s*\{([^}]*)\}\s*\{([^}]*)\}\s*(?:triggers\s+([^\n]*))?',text):
        transitions[m[2]]=(m[3].split(),m[4],m[5],m[6],bool(m[1]))
    queries=[(m[1].split(),m[2]) for m in re.finditer(r'unsafe\s*\(([^)]*)\)\s*\{([^}]*)\}',text)]
    state={m[1]:0 for m in re.finditer(r'var\s+(\w+)\s*:',text)}
    state.update({m[1]:{v:0 for v in DOMAIN} for m in re.finditer(r'array\s+(\w+)\[proc\]',text)})
    init=re.search(r'init\s*\(p\)\s*\{([^}]*)\}',text)[1]
    # Initialize equality atoms to their literal RHS; symbolic data initially 0.
    for atom in init.split('&&'):
        lhs,rhs=atom.strip().split('=',1)
        val=expr(rhs,state,{'p':0})
        if '[p]' in lhs:
            state[lhs.split('[')[0]]={v:val for v in DOMAIN}
        else: state[lhs.strip()]=val
    return transitions,queries,state

def execute(path,steps):
    transitions,queries,state=parse(path)
    allowed=None; trace=[]
    for name,values in steps:
        args,pre,updates,calls,triggered=transitions[name]
        assert len(values)==len(args) and len(set(values))==len(values),(name,'arity/distinctness')
        if allowed is None: assert not triggered,(name,'triggered from boundary')
        else:
            assert any(n==name and len(vs)==len(values) and all(x is None or x==v for x,v in zip(vs,values)) for n,vs in allowed),(name,'not permitted CFG successor',allowed)
        bind=dict(zip(args,values)); assert guard(pre,state,bind),(name,'guard',state)
        changes={}
        for assignment in updates.split(';'):
            if not assignment.strip(): continue
            lhs,rhs=map(str.strip,assignment.split(':=',1))
            cell=re.fullmatch(r'(\w+)\[(\w+)\]',lhs)
            if cell:
                arr,index=cell.groups(); dest=dict(state[arr])
                if rhs.startswith('case'):
                    branches=[tuple(map(str.strip,b.split(':',1))) for b in rhs[4:].split('|') if b.strip()]
                    for v in DOMAIN:
                        b={**bind,index:v}
                        for condition,value in branches:
                            if condition=='_' or expr(condition,state,b):
                                dest[v]=expr(value,state,b);break
                else: dest[bind[index]]=expr(rhs,state,bind)
                changes[arr]=dest
            else: changes[lhs]=expr(rhs,state,bind)
        state.update(changes)
        allowed=None if not calls else [(n,[None if x=='_' else bind[x] for x in actual.split()]) for n,actual in re.findall(r'(\w+)\(([^)]*)\)',calls)]
        trace.append({'transition':name,'arguments':values,'boundary':allowed is None})
    violations=[]
    for number,(args,pred) in enumerate(queries,1):
        for vals in itertools.permutations(DOMAIN,len(args)):
            if guard(pred,state,dict(zip(args,vals))): violations.append({'query':number,'arguments':vals});break
    return {'file':path.name,'completed_boundary':allowed is None,'violations':violations,'trace':trace}

def flow(kind,r,owner=None,victims=(),premature=False,store=None):
    f='m' if owner is None else 'o'; args=(r,) if owner is None else (r,owner)
    out=[('issue_'+kind,(r,)),(f+'_accept_'+kind+('_home' if r==0 else '_remote'),args),(f+'_step',args)]
    if store is not None: out += [(f+'_store',args+(store,)),(f+'_step',args)]
    out += [(f+'_reply',args),(f+'_step',args),(f+'_receive',args),(f+'_step',args)]
    for v in victims: out += [(f+'_inv',args+(v,)),(f+'_step',args)]
    if not premature:
        for v in reversed(victims): out += [(f+'_ack',args+(v,)),(f+'_step',args)]
    out += [(f+'_finish_'+('x' if kind=='GetX' else 's'),args)]
    if kind=='Get': out += [('delayed_put',(r,))]
    return out

def main():
    setup=flow('Get',1)+flow('Get',2)
    records=[]
    for name,steps in [
        ('flash-acquire-strict.cub',setup+flow('GetX',0,victims=(1,2))),
        ('flash-completion-witness.cub',setup+flow('GetX',0,victims=(1,2))),
        ('flash-premature-grant.cub',setup+flow('GetX',0,victims=(1,2),premature=True)),
        ('flash-ignored-invalidation.cub',setup+flow('GetX',0,victims=(1,2))),
        ('flash-stale-owner.cub',flow('GetX',0)+flow('GetX',3,owner=0,store=4))]:
        record=execute(HERE/name,steps)
        assert record['completed_boundary']
        assert bool(record['violations'])==(name!='flash-acquire-strict.cub'),record
        records.append(record)
    ack_first=flow('GetX',0,victims=(2,1))
    payload_steps=ack_first[3:7]
    ack_first=ack_first[:3]+ack_first[7:-1]+payload_steps+ack_first[-1:]
    supplementary=[
        execute(HERE/'flash-acquire-strict.cub',flow('GetX',0)+flow('GetX',3,owner=0,store=4)),
        execute(HERE/'flash-completion-witness.cub',setup+ack_first)]
    assert not supplementary[0]['violations'] and supplementary[1]['violations']
    out=HERE/'.local/concrete-replays.json';out.parent.mkdir(exist_ok=True)
    out.write_text(json.dumps({'domain':DOMAIN,'results':records,'supplementary':supplementary},indent=2)+'\n')
    print(json.dumps([{'file':r['file'],'steps':len(r['trace']),'boundary':r['completed_boundary'],'violations':r['violations']} for r in records],indent=2))
if __name__=='__main__':main()
