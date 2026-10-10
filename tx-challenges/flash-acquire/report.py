"""Render all retained execution records, including development failures."""
from pathlib import Path
import json
HERE=Path(__file__).resolve().parent
rows=[json.loads(x) for x in (HERE/'.local/results.jsonl').read_text().splitlines()]
header='\n## Retained execution results\n\nAll rows, including earlier failed revisions. Input SHA256 in JSONL distinguishes them.\n\n| Model | Input hash | Mode | Outcome | Exit | Wall seconds | Visited | Forward | Solver calls | Invariants | Restarts | Max proc |\n|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|\n'
def val(x): return '—' if x is None else str(x)
def recipe(r):
    result=r['mode']; argv=r['argv']
    for flag in ('-brab','-search','-forward-depth'):
        if flag in argv: result+=' / '+flag[1:]+':'+argv[argv.index(flag)+1]
    return result
for r in rows:
 header+='| '+ ' | '.join([r['model'],r['input_sha256'][:12],recipe(r),r['outcome'],val(r['exit_code']),f"{r['wall_seconds']:.3f}",*[val(r[k]) for k in ['visited_nodes','forward_nodes','solver_calls','invariants','restarts','max_processes']]])+' |\n'
notes=HERE/'NOTES.md'; text=notes.read_text().split('\n## Retained execution results')[0];notes.write_text(text+header)
print(json.dumps([{'file':r['model'],'mode':r['mode'],'verdict':r['outcome'],'nodes':r['visited_nodes'] if r['visited_nodes'] is not None else -1,'seconds':r['wall_seconds']} for r in rows],indent=2))
