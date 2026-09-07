# Cubicle agent guide

Cubicle is an OCaml model checker for safety properties of parameterized systems.
Use Kes in conversation and documents; preserve literal filesystem paths.

## Start here

Read the task specification and current Git status, then only the interfaces,
implementations, and callers relevant to this phase. Load ocaml-cubicle-development
for source work and cubicle-task-execution for assigned tasks. Before correctness-sensitive edits or
changes to generated sources, read .hermes/tetra-v2/project-guide.md. For a refactor,
also load house-style-refactoring. These instructions apply to all agents here.

The task specification is the approved description of the goal and required work.
Kes and Tetra alone amend it. Workers ask Tetra for missing instructions and report
inadequacies rather than changing assumptions or research objectives. An approved
experiment may legitimately yield a negative or inconclusive result.

User instructions and the approved task specification may override generic skill
procedures. Workers report real conflicts to Tetra rather than changing the
specification themselves. This rule concerns procedure only: it grants no new
permissions and does not override platform restrictions.

## Working rules

- Protect soundness. A build or passing test is evidence, not a proof of semantics.
- Inspect definitions and callers before technical claims or edits. State assumptions
  and unresolved proof obligations; consult primary sources for mathematical claims.
- Make small, coherent changes in the observed OCaml style. Preserve source diagnostics,
  including the Typing.Error boundary, unless the approved task changes them.
- Treat existing modifications and untracked files as user work. Do not overwrite,
  discard, stage, or commit unrelated changes. Resolve overlapping edits with Tetra.
- One principal owns writes and builds in each workspace at a time. First-round
  implementation and independent test design may run in separate approved
  workspaces from the same starting commit. Do not inspect the other workspace
  during blind test design. Finish edits, builds,
  and subagents before handoff. Do no more repository work after handing over.
- Isoqa's approval goes to Tetra for final review. Only Tetra marks the task complete.
  If Octa believes Isoqa's tests conflict with the specification, it stops the task
  and reports to Tetra. If Tetra rejects an Isoqa approval, Tetra reports the failure
  to Kes and proposes another run. Neither failure starts an automatic repair loop.
- On a branch and task approved by Tetra, Octa may commit implementation changes
  but not tests or experimental files. Isoqa alone designs and commits test suites.
  Octa may run existing tests and temporary implementation experiments; report their
  results and remove its disposable files before handoff. Isoqa may commit tests
  and its report. Otherwise commits need explicit permission.
  Merges, pushes, history rewrites, destructive operations, permission expansion,
  model/provider changes, and external integrations require separate authorization.
- Never expose or commit credentials. Read secrets only through their intended tools
  when an authorized operation needs them; do not inspect credential file contents.
- Use plain language, explicit actors, and technical terms defined when needed.
  Report observed results and material gaps, not invented output or promises.

## Build and test

Use the existing Autoconf/Make build. On this macOS setup:

```sh
eval "$(/opt/homebrew/bin/opam env)"
make
make test
./cubicle.opt <options> <model.cub>
```

Configure only when needed. Use make depend when module dependencies change.
Edit generator inputs: parser.mly, lexer.mll, muparser.mly, mulexer.mll, and
Makefile.in. The detailed generated-file rules are in project-guide.md.

Run focused checks first and make test for production changes. Isoqa's new feature models
belong in tests/<feature>/ with meaningful names, expected outcomes, and a documented
run command; keep existing examples regressions working. Check output and exit
status. Distinguish timeouts, crashes, input errors, and unknown results from verdicts.
Before submission, review the diff, run git diff --check, and report exactly what
was tested and what remains unverified. Routine reports need concise commands and
results; add environment details when relevant to an experiment or failure.

## Find relevant knowledge

- kb/architecture.md: execution and verification call map.
- kb/coding-style.md: handwritten OCaml conventions.
- kb/scientific-foundations.md: scientific sources and assumptions.
- Transactions: use kb/transactions.md for the implementation map and
  kb/transactional-mcmt.md for semantic questions. Read transaction.mli/ml and
  relevant callers when the change affects them; consult kb/todo.md for unresolved
  questions. A documentation-only correction does not require source re-onboarding.
  Preserve behavior unless the approved task changes it.
- .hermes/tetra-v2/project-guide.md: detailed module map, semantics-sensitive
  checks, generated files, transaction guidance, and ownership rules.

The repository and primary literature are authoritative. KB notes and memory are
retrieval aids; check their sources. Follow kb/README.md when maintaining knowledge.
