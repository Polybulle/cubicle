"""Generate the acquisition specification and minimal negative controls."""
from pathlib import Path
import re
ROOT=Path(__file__).resolve().parent
head='''(* FLASH-inspired full-data completed-acquisition specification, not a proved
   reduction of examples/flash.cub. One line, distinguished Home represented by
   the same physical cache arrays as remotes, arbitrary proc population, symbolic
   proc-tag data. Single-slot reliable messages. No failure/liveness claim.
   Run: python3 tx-challenges/flash-acquire/run.py
   Transaction accepts Get/GetX, binds requester and old owner, branches to memory
   or owner forwarding, recursively invalidates victims and consumes actual acks,
   receives payload in either order, then grants/releases. Environment choices
   are explicit internal successors; this is one globally exclusive CFG flow.
   STRICT is DELAYED-style: grant only when actual ack obligations are empty.
   Stores, PutX and Replace are ordinary boundary transitions as well as explicit
   internal owner-store/writeback/replacement choices. No global freeze flag.
   EAGER companion instead retains original flash.cub executable rules/properties.
*)
type permission = I | S | E
type command = Empty | Get | GetX | Put | PutX | Nak
type inv = NoInv | Inv | Ack
type phase = Idle | Accepted | Working | Received
var Home : proc
var Phase : phase
var Requester : proc
var OldOwner : proc
var FromOwner : bool
var Owner : proc
var Dirty : bool
var Kind : command
var MemData : proc
var CurrData : proc
var PrevData : proc
var Payload : proc
var Ready : bool
var Collecting : bool
var Completed : bool
var NakcMsg : bool
var WbValid : bool
var WbOwner : proc
var WbData : proc
var ShWbValid : bool
var ShWbData : proc
array Cache[proc] : permission
array Data[proc] : proc
array Sharer[proc] : bool
array Pending[proc] : bool
array Required[proc] : bool
array InvMsg[proc] : inv
array UniMsg[proc] : command
array UniData[proc] : proc
array InvMarked[proc] : bool
array Revoked[proc] : bool
init (p) {
Phase = Idle && FromOwner = False && Dirty = False && Kind = Empty && MemData = CurrData &&
PrevData = CurrData && Ready = False && Collecting = False && Completed = False &&
NakcMsg = False && WbValid = False && ShWbValid = False &&
Cache[p] = I && Sharer[p] = False && Pending[p] = False && Required[p] = False &&
InvMsg[p] = NoInv && UniMsg[p] = Empty && InvMarked[p] = False && Revoked[p] = False
}
(* Separate home/remote and remote/remote physical exclusion queries. *)
unsafe (h p) { h = Home && Cache[h] = E && Cache[p] <> I }
unsafe (h p) { h = Home && Cache[p] = E && Cache[h] <> I }
unsafe (p q) { p <> Home && q <> Home && Cache[p] = E && Cache[q] <> I }
unsafe (p) { Cache[p] <> I && Data[p] <> CurrData }
unsafe () { Dirty = False && MemData <> CurrData }
unsafe (p) { Completed = True && Pending[p] = True }
unsafe (p) { Completed = True && Required[p] = True && Cache[p] <> I }
(* PROPERTY END *)
'''
parts=[head]
def tr(name,args,guard,updates,calls='',triggered=False):
    if triggered and name.startswith(('m_','o_')):
        guard = 'FromOwner = '+('True && OldOwner = o' if name.startswith('o_') else 'False')+' && '+guard
    if '_accept_' in name:
        updates += '; FromOwner := '+('True' if name.startswith('o_') else 'False')
    parts.append(('triggered\n' if triggered else '')+f'transition {name} ({args})\nrequires {{ {guard} }} {{\n'+updates+'\n}\n'+('triggers '+calls+'\n' if calls else '')+'\n')
# Request issue is outside acceptance; delayed shared replies can cross flows.
for kind in ('Get','GetX'):
 tr('issue_'+kind,'r',('UniMsg[r] = Empty && Cache[r] = I' if kind=='Get' else 'UniMsg[r] = Empty && Cache[r] <> E'),f'UniMsg[r] := {kind}')
tr('store','p d','Cache[p] = E','Data[p] := d; CurrData := d')
tr('putx','p','Cache[p] = E && WbValid = False','Cache[p] := I; WbValid := True; WbOwner := p; WbData := Data[p]')
tr('writeback','', 'WbValid = True','MemData := WbData; Dirty := False; WbValid := False')
tr('replace','p','Cache[p] = S && UniMsg[p] = Empty','Cache[p] := I; Sharer[p] := False')
tr('delayed_put','p','UniMsg[p] = Put','Cache[j] := case | j = p && InvMarked[p] = False : S | _ : Cache[j]; Data[p] := UniData[p]; UniMsg[p] := Empty; InvMarked[p] := False')
tr('collision','p','Phase <> Idle && UniMsg[p] = Get && p <> Requester','UniMsg[p] := Nak')
tr('collision_x','p','Phase <> Idle && UniMsg[p] = GetX && p <> Requester','UniMsg[p] := Nak')
tr('nak_receive','p','UniMsg[p] = Nak','UniMsg[p] := Empty; InvMarked[p] := False')
# Distinct accepted roles. Clean upgrade with an extant peer retained separately.
for fam,owner in [('m',False),('o',True)]:
 args='r o' if owner else 'r'
 bind='r o' if owner else 'r'
 def call(n,extra=''): return f'{fam}_{n}({bind}{" " if extra else ""}{extra})'
 common='Phase = Idle && WbValid = False'
 for kind in ('Get','GetX'):
  for locality in ('home','remote'):
   guard=common+f' && UniMsg[r] = {kind} && '+('r = Home' if locality=='home' else 'r <> Home')+' && '+('Dirty = True && Owner = o' if owner else 'Dirty = False')
   updates=f'Phase := Accepted; Requester := r; Kind := {kind}; Ready := False; Completed := False; PrevData := CurrData; Revoked[j] := case | _ : False; Required[j] := case | j <> r && Sharer[j] = True && UniMsg[r] = GetX : True | _ : False; Pending[j] := case | j <> r && Sharer[j] = True && UniMsg[r] = GetX : True | _ : False; InvMsg[j] := case | j <> r && Sharer[j] = True && UniMsg[r] = GetX : Inv | _ : NoInv'
   if owner: updates+='; OldOwner := o'
   tr(f'{fam}_accept_{kind}_{locality}',args,guard,updates,call('step'))
 if not owner:
  # Source's head=requester + additional peer branch, not dropped.
  tr('m_accept_upgrade_peer','r v',common+' && Dirty = False && UniMsg[r] = GetX && Sharer[r] = True && Sharer[v] = True', 'Phase := Accepted; Requester := r; Kind := GetX; Ready := False; Completed := False; PrevData := CurrData; Revoked[j] := case | _ : False; Required[j] := case | j <> r && Sharer[j] = True : True | _ : False; Pending[j] := case | j <> r && Sharer[j] = True : True | _ : False; InvMsg[j] := case | j <> r && Sharer[j] = True : Inv | _ : NoInv',call('step'))
 successors=' or '.join([call('reply'),call('receive'),call('inv','_'),call('ack','_'),call('finish_x'),call('finish_s'),call('replace','_'),call('collision','_'),call('collision_x','_'),call('late_put','_')]+([call('store','_'),call('evict'),call('nak')] if owner else []))
 # Administrative dispatch is identity in data but changes phase at acceptance.
 tr(f'{fam}_step',args,'Phase <> Idle && Requester = r','Phase := Working',successors,True)
 g='Phase = Working && Ready = False && Requester = r'
 if owner:
  tr(f'{fam}_reply',args,g+' && Cache[o] = E','Payload := Data[o]; Ready := True; Cache[j] := case | j = o && Kind = GetX : I | j = o && Kind = Get : S | _ : Cache[j]; Sharer[j] := case | j = o && Kind = Get : True | j = o && Kind = GetX : False | _ : Sharer[j]; ShWbValid := True; ShWbData := Data[o]; Pending[o] := False; InvMsg[o] := NoInv',call('step'),True)
  tr(f'{fam}_store',args+' d',g+' && Cache[o] = E','Data[o] := d; CurrData := d',call('step'),True)
  tr(f'{fam}_evict',args,g+' && Cache[o] = E','Cache[o] := I; WbValid := True; WbOwner := o; WbData := Data[o]',call('step'),True)
  tr(f'{fam}_nak',args,g+' && Cache[o] <> E','UniMsg[r] := Nak; NakcMsg := True; Phase := Idle; Pending[j] := case | _ : False; Required[j] := case | _ : False; InvMsg[j] := case | _ : NoInv',triggered=True)
 else:
  tr(f'{fam}_reply',args,g,'Payload := MemData; Ready := True',call('step'),True)
 tr(f'{fam}_receive',args,'Phase = Working && Requester = r && Ready = True && UniMsg[r] <> PutX','UniMsg[r] := PutX; UniData[r] := Payload',call('step'),True)
 tr(f'{fam}_inv',args+' v','Phase = Working && Requester = r && InvMsg[v] = Inv','Cache[v] := I; Sharer[v] := False; InvMsg[v] := Ack; InvMarked[j] := case | j = v && UniMsg[v] = Put : True | _ : InvMarked[j]; Revoked[v] := True',call('step'),True)
 tr(f'{fam}_ack',args+' v','Phase = Working && Requester = r && InvMsg[v] = Ack && Pending[v] = True','Pending[v] := False; InvMsg[v] := NoInv',call('step'),True)
 tr(f'{fam}_replace',args+' v','Phase = Working && Requester = r && Cache[v] = S && UniMsg[v] = Empty','Cache[v] := I; Sharer[v] := False',call('step'),True)
 for k in ('Get','GetX'):
  tr(f'{fam}_collision'+('_x' if k=='GetX' else ''),args+' v',f'Phase = Working && Requester = r && UniMsg[v] = {k}','UniMsg[v] := Nak',call('step'),True)
 tr(f'{fam}_late_put',args+' v','Phase = Working && Requester = r && UniMsg[v] = Put','Cache[j] := case | j = v && InvMarked[v] = False : S | _ : Cache[j]; Data[v] := UniData[v]; UniMsg[v] := Empty; InvMarked[v] := False',call('step'),True)
 # Empty obligation guard, not a restatement of physical coherence.
 tr(f'{fam}_finish_x',args,'Phase = Working && Requester = r && Kind = GetX && UniMsg[r] = PutX && Pending[r] = False'+(' && Pending[o] = False' if owner else '')+' && forall_other j. Pending[j] = False','Cache[r] := E; Data[r] := UniData[r]; UniMsg[r] := Empty; Owner := r; Dirty := True; Phase := Idle; Completed := True; ShWbValid := False',triggered=True)
 # Get returns a delayed reply at a boundary: directory records future sharer.
 tr(f'{fam}_finish_s',args,'Phase = Working && Requester = r && Kind = Get && UniMsg[r] = PutX','UniMsg[r] := Put; Sharer[r] := True; MemData := Payload; Dirty := False; Phase := Idle; ShWbValid := False',triggered=True)
text=''.join(parts)
(ROOT/'flash-acquire-strict.cub').write_text(text)
(ROOT/'flash-premature-grant.cub').write_text(re.sub(r' && Pending\[r\] = False(?: && Pending\[o\] = False)? && forall_other j\. Pending\[j\] = False','',text))
(ROOT/'flash-stale-owner.cub').write_text(text.replace('Payload := Data[o]','Payload := PrevData'))
(ROOT/'flash-ignored-invalidation.cub').write_text(text.replace('Cache[v] := I; Sharer[v] := False; InvMsg[v] := Ack','Cache[v] := Cache[v]; Sharer[v] := False; InvMsg[v] := Ack'))
a=text.index('(* Separate home/remote')
b=text.index('(* PROPERTY END *)')
w=text[:a]+'''(* Positive reachability, NOT a safety claim: two distinct victims in one flow. *)
unsafe (r v w) { Completed = True && Requester = r && Cache[r] = E && Revoked[v] = True && Revoked[w] = True }
'''+text[b:]
(ROOT/'flash-completion-witness.cub').write_text(w)
source=(ROOT.parents[1]/'examples/flash.cub').read_text()
counts={}
def rename(m):
 n=m.group(1); counts[n]=counts.get(n,0)+1
 return 'transition '+n+(f'__{counts[n]}' if counts[n]>1 else '')
source=re.sub(r'transition\s+(\w+)',rename,source)
(ROOT/'flash-acquire-eager.cub').write_text('(* SOURCE-FAITHFUL EAGER reference. Original full-data flash.cub rules and\n   properties; only duplicate transition declarations renamed. Unannotated: this\n   companion does NOT claim a completed-flow transaction or strict E/S contract.\n   Early PutX and Collecting/PrevData retained verbatim. *)\n'+source)
print('Generated six models; EAGER duplicate counts:',{k:v for k,v in counts.items() if v>1})
