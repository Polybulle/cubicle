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