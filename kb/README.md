# Cubicle project notes

This directory holds concise notes that are useful across work sessions and expensive to reconstruct from scratch. It is not intended to mirror the source tree or become a complete encyclopedia of Cubicle.

The source code, executed checks, and primary literature remain authoritative.

## What belongs here

- an architecture map that helps navigate the implementation;
- a paper note tied to code or an active research question;
- a non-obvious invariant or algorithm-to-code correspondence;
- a reproducible experiment and its interpretation;
- a design decision whose rationale would otherwise be lost.

Do not add routine session summaries, copied source, raw logs, generated output, speculative explanations, or facts that are easy to recover with a search.

Keep the layout flat until enough real notes exist to justify subdirectories. Use descriptive filenames such as `architecture.md`, `paper-cubicle.md`, or `experiment-<topic>.md`.

## Current map

| Note | Role |
|---|---|
| `architecture.md` | Current generic execution, symbolic backward-reachability, and BRAB call path. |
| `coding-style.md` | Source-grounded guide to Cubicle's OCaml error, module, interface, and implementation conventions. |
| `scientific-foundations.md` | Published Cubicle/BRAB theory and its stated assumptions. |
| `transactions.md` | Current-source map for triggers, `part` elaboration, `-tx` modes, and recorded smoke checks. |
| `mcmt-notes.md` | ATS/TATS formal vocabulary and safety-observation hierarchy. |
| `transactional-mcmt.md` | Primary onboarding note: submitted TxCubicle paper semantics, algorithms, implementation correspondence, and engineering checklist. |
| `todo.md` | Specified transactional work that is not yet implemented or proved. |
| `questions.md` | Current open questions; presently empty. |

Use **transactional MCMT** as the umbrella term. **TxCubicle** is the submitted paper's name for the extension. A **transaction path** is an initial-to-final trigger path. `part` denotes the union of serial part permutations; it is a candidate for removal.

## Minimal note format

Each note should say:

```markdown
# Topic

Checked: <date and/or commit, paper version, or experiment run>
Sources: <paths and symbols, paper sections, or commands>

<concise findings>

## Open questions

<uncertainties, if any>
```

Mark hypotheses explicitly. When a source and a note disagree, correct or mark the note as stale; do not treat the note as evidence.

Holographic memory may store a short pointer to a useful note or source-grounded fact. Detailed reasoning belongs here or in the primary source, not only in memory.