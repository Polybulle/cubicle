# Ordinary backward search with transaction options

A system is ordinary only if every typed transition is untriggered, may yield,
and has no successor calls. Its CFG consists of independent executable steps
between neutral states. The backward relation is therefore the ordinary union of
transition pre-images, including the same participant instantiations, universal
guards, simplifications, and postponement as `-tx none`.

At a neutral input, `Pre.pre_image` uses that existing ordinary algorithm under
`-tx bwd/all` as well. Located inputs still use CFG dispatch, including inputs
supplied through the public API rather than produced by ordinary exploration.
This does not bypass CFG exploration for annotated systems, including mixed
ordinary/transaction systems, recursive entrypoints, or non-yielding transitions.
It restores the ordinary predecessor order rather than trying to compensate for
the different work performed by CFG instantiation.

Ordinary predecessors are created at the neutral position. Their executable
transition and actual arguments remain in the history; ancestor objects and
formula variables are not rewritten. This also avoids copying an already
canonical ordinary node merely to discard an unused control position.

Run from the configured checkout in the opam environment:

    make
    make -f Makefile -f tests/ordinary-transitions/check.mk ordinary-transitions-check
    python3 tests/ordinary-transitions/run.py

The unit probe checks the strict annotation predicate, bypass of CFG dispatch,
neutral positions, history identity/depth, normalization identity, and identical
ordered predecessor outputs. The integration runner checks SAFE and UNSAFE cases
across all four modes, BFS/DFS, all postponement policies, and deletion on/off.
It compares counters and executable traces, not just verdicts. The existing
Flash SAFE/UNSAFE models also run with their benchmark BRAB-2/depth-6 recipe in
all four modes, comparing counters and traces. This guards the ordinary fast
path on models with real approximation and array updates. The existing
CFG-boundary, located-covering, forward, scheduler, and gapped-witness suites
exercise the annotated path that must remain unchanged. These checks are
regression evidence, not a general proof of the checker.
