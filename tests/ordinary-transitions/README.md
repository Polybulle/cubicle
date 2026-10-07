# Neutral ordinary predecessors

Ordinary backward pre-image computation creates predecessors at the neutral
position rather than attaching an unused transition control position. Their
executable transition and actual arguments remain in history. Cube variables,
ancestor objects, and trace depth are preserved. An already canonical node can
therefore be reused by `Node.normalize` instead of copied merely to discard its
control position. The CFG pre-image path still creates located intermediate nodes.

Run from the configured checkout in the opam environment:

    make
    make -f Makefile -f tests/ordinary-transitions/check.mk ordinary-transitions-check
    python3 tests/ordinary-transitions/run.py

The unit probe checks neutral positions, history identity/depth, and normalization
identity. The integration runner checks ordinary SAFE/UNSAFE cases across BFS/DFS,
all postponement policies, and deletion on/off. These checks are regression
evidence, not a general proof of the checker.
