# Transactional MCMT TODO

## Deliberately deferred

- New work on `part`: its current serial-permutation semantics is accepted, but
  the feature is a candidate for removal.
- Reworking the forward candidate-filtering heuristic: intermediate transaction
  states remain intentionally recorded for negative candidate filtering; see
  `kb/transactional-mcmt.md`.
