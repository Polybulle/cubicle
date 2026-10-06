# Direct Tetra validation — specification r3

## Status

Tetra resumed execution directly at Kes's request. No further subagents, role
handoffs, or Kanban were used. No production or reporting changes were made.
Production remains fa414c97896cea8a7625986fad0070276fa7d937. Prior reviewed HEAD
was 4585977759147a57a57aca7b94967e821376f417. This report is not a new commit or
final acceptance. The original frozen tests and historical reviews are unchanged.

## Real parallel capability resolved

Installed into the existing switch 4.12.0, not the active switch:

```
/opt/homebrew/bin/opam install --switch=4.12.0 --yes --assume-depexts functory num
/opt/homebrew/bin/opam exec --switch=4.12.0 -- ocamlc -version
/opt/homebrew/bin/opam exec --switch=4.12.0 -- ocamlfind query functory
```

Installed conf-autoconf 1, functory 0.6, num 1.6, ocamlfind 1.9.8, with no compiler
change or package removal. autoconf and automake already existed under
/opt/homebrew/bin; no system package installation was run. Opam recorded a depext
bypass for those two tools in switch 4.12.0. The active switch remained 5.0.0.
Verified compiler 4.12.0 and Functory library path
/Users/hector/.opam/4.12.0/lib/functory.

A fresh git archive of reviewed HEAD was built under the ignored task directory:
`.hermes/tetra-v2/tasks/state-dispatch-scheduler/direct/parallel-412/`.
Existing 5.4.1 binaries/build products were not overwritten. Commands in that copy:

```
opam exec --switch=4.12.0 -- sh -c 'autoconf && ./configure && make'
opam exec --switch=4.12.0 -- make -f Makefile -f tests/state-dispatch-scheduler/probe.mk isoqa-probe
python3 tests/state-dispatch-scheduler/run-probes.py --output ../parallel-probes
python3 tests/state-dispatch-scheduler/run.py --matrix --output ../parallel-matrix
opam exec --switch=4.12.0 -- make test
opam exec --switch=4.12.0 -- make -f Makefile -f tests/state-dispatch-scheduler/review.mk isoqa-review-approx
timeout 45 tests/state-dispatch-scheduler/.local/review-approx.opt -nocolor -quiet -tx bwd -brab 2 -j 2 -depth 100 -nodes 100000 examples/german.cub
```

All opam invocations used /opt/homebrew/bin/opam. Build succeeded and the link
command uses functory.cmxa from the 4.12.0 library, not the fake module. A linker
warning about nonexistent optional lib/ocaml/unix search path did not prevent
linking. Results:

- 13/13 structural probes pass, including all three previously blocked j2 probes.
- 268/268 feature checks match, including the 12 j2 invocations. The zero-step j2
  case still does not by itself prove parallel work; the other cases/probes do.
- make test succeeds, 14 OK.
- Parallel approximation probe succeeds: internal=1294, boundary=56,
  candidate_dispatch=35, candidates=35. The probe checks neutral candidate/visited
  positions while exercising actual candidate selection.

Detailed matrix/probe results are in direct/parallel-matrix/results.json and
direct/parallel-probes/results.json. Fresh build log is direct/build-412.log.
These are tests, not a mathematical soundness proof.

## Broader performance investigation

The direct harness is `direct/larger_bench.py` under the same ignored task
directory. It retains commands, outcomes, hashes, timing, RSS and counts after
every run in each batch's runs.jsonl, with raw output in separate logs. It uses
the existing matched baseline/candidate 5.4.1 executables from the prior review;
no 4.12-vs-5.4 comparison is made. The baseline is 1663f808d30a39580fbab671a37da143960a779e.
The binary hashes are in every batch metadata.json. All performance runs are
serial; no builds or other benchmark were run concurrently by Tetra.

The three variants are baseline without tx, candidate without tx, candidate with
-tx bwd. Every run uses -nocolor, explicit depth and node limits, an external wall
timeout, and /usr/bin/time -l. Model-specific BRAB/forward options are recorded.
The same forward options are used across variants; bwd does not enable the tx
forward oracle. Bare -tx was not repeated in this broader set. Raw log files are
written during timing (unlike the prior pipe-based diagnosis); results must not
be directly combined with that diagnosis's timings. Desktop-host noise remains.

Screening covered 14 model/option cases, not German alone: FLASH variants,
hierarchical snoop, Chandra–Toueg, non-atomic Szymanski and German with data. In
all, direct runs.jsonl files contain 117 executions: 35 SAFE, 3 UNSAFE, 57 explicit
LIMIT, 18 external TIMEOUT, and 4 ERROR. These include repeated runs, not 117
independent models. Source size was used only to discover candidates; several
large-source models finish too quickly with BRAB to assess loop overhead.

### Compatibility limitations found during screening

flash.cub, flash_nodata.cub, flash_abstr.cub, and
challenges/hierarchical_snoop_cygc_nodata_invs.cub reject -tx bwd because their
transition names are not unique. The FLASH no-data rejection was reproduced on
the exact baseline (exit 2), so this is not introduced by the scheduler patch.
No supplied model was edited or silently renamed. Rejected runs are not timings
of verification. Non-tx FLASH/hierarchical searches and other unassisted searches
hit the initial 20-second cutoff. Unique-name challenges/flash2.cub and
challenges/hierarchical_snoop_cygc_nodata.cub still timed out in all three variants
at 45 seconds with depth 100. Equal timeouts establish no performance ratio.

### Repeated heavier bounded searches

To measure completed bounded search rather than equal wall cutoffs, ran:

```
python3 direct/larger_bench.py --models flash2 hierarchical_plain --depth 5 --reporting both --repetitions 3 --timeout 30 --output direct/depth5-comparison
python3 direct/larger_bench.py --models german_data_brab --reporting both --repetitions 3 --timeout 30 --output direct/data-comparison
```

Here direct/ denotes the full ignored task-directory prefix. FLASH2 and
hierarchical runs all return LIMIT at the specified depth, not SAFE/UNSAFE.
German data uses examples/german_pfs_data.cub with -brab 2, depth 100 and reaches
SAFE. Medians in seconds; tx overhead is candidate-bwd versus candidate ordinary:

| Case | Output | Baseline | Candidate ordinary | Candidate bwd | Tx overhead |
|---|---|---:|---:|---:|---:|
| FLASH2, depth 5 | quiet | 1.428 | 1.427 | 1.492 | +4.58% |
| FLASH2, depth 5 | normal | 1.431 | 1.587 | 1.694 | +6.71% |
| Hierarchical snoop, depth 5 | quiet | 4.155 | 4.270 | 4.524 | +5.94% |
| Hierarchical snoop, depth 5 | normal | 4.235 | 4.241 | 4.879 | +15.06% |
| German data, BRAB 2 | quiet | 2.892 | 2.747 | 2.840 | +3.39% |
| German data, BRAB 2 | normal | 3.156 | 2.835 | 3.274 | +15.49% |

FLASH's first normal-output comparison had noisy ordinary timings and a +10.93%
baseline-to-candidate median. It was investigated, not silently dropped: a separate
five-repetition recheck (direct/flash-normal-recheck) gave baseline 1.472913,
candidate 1.474380 (+0.10%), bwd 1.643396 (+11.46% versus candidate). Both batches
remain in the evidence. These small samples do not justify declaring exact
performance equality or treating every observed delta as an implementation cost.

All baseline/candidate ordinary verifier outputs in the initial repeated batches
were identical in each pair after removing only time(1)'s trailer. Counts and
traces therefore match where printed. Examples of bounded-search work:

- FLASH2 ordinary: 635 expansions, 3,905 fixpoints, 40,690 solver calls;
  bwd: 5,160 expansions, 3,892 fixpoints, 40,452 solver calls.
- Hierarchical ordinary: 328 expansions, 7,259 fixpoints, 57,295 solver calls;
  bwd: 7,906 expansions, 7,252 fixpoints, 57,270 solver calls.

The expansion counter is not visited-trie membership. Depth limits and changed
search order mean these are bounded-search workloads, not proven identical
amounts of semantic work or full verification performance.

## Interpretation and remaining work

Larger examples support a narrower conclusion than the German-only diagnosis:
quiet backward traversal adds a measured few percent in these bounded/completed
samples, while normal reporting remains material on hierarchical snoop and
German data. Reporting is not assumed to dominate all larger models. No reporting
suppression or special nontransactional fast path was implemented.

Parallel capability and its previously failing required checks are now resolved.
Full large-model verification performance remains inconclusive: heavy unassisted
runs timed out, and depth-bounded results cannot replace full-verdict timings.
No numeric overhead acceptance threshold has been invented. Prior frozen tests,
reports, and partial evidence remain preserved. No new commits, merges or pushes
were performed. Kes's checkout and untracked examples/tests remain untouched.
