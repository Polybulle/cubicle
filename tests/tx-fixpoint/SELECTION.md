# Dynamic storage selection (spec r2)

Status: implemented and tested; independent amendment review pending.

Kes requested that ordinary backward search use the normal cube trie rather than
an unconditional located-store wrapper. `Cubetrie.Selected` now chooses once at
module initialization, using the existing first-class-module style:

- `Options.tx_bwd = false`: `Cubetrie.Ordinary`, whose `t` is exactly
  `Node.t Cubetrie.t`. There is no location map or retained transaction-node list.
- `Options.tx_bwd = true`: `Cubetrie.Located`, retaining located obligations and
  its separate compressed lookup index.

The ordinary adapter reuses the normal trie operations. Node insertion and deletion
normalize data keys, preserving the preceding implementation's gapped-variable
repair and original node identities. Generic data-only trie operations remain
available. Both backward schedulers, quick/hard trie checks, and the naive checker
use the same selected module/type. Covering remains in Fixpoint. Query views remain;
this amendment changes storage selection, not the normalization contract.

The ordinary branches inside Located were removed. Transaction storage behavior
is unchanged. Tests distinguish the selected representations through ordinary
compression versus transaction retention, and statically check Ordinary.t against
Node.t Cubetrie.t. Direct probes exercise all five transaction modes.

## Executed verification

- `make depend`, `make`, `make test`: passed.
- Main focused suite: 93 executions passed.
- Fresh real-Functory build (OCaml 4.12.0), two cores: 93 executions passed.
- Fresh Z3 build (OCaml 5.4.1): 92 executions passed, with the existing backend
  exclusions stated in README.md.
- Existing located-covering and neutral-candidates suites: rebuilt and passed
  sequentially after the amendment.
- Scoped `git diff --check`: passed.

The 278 focused executions were counted from their JSON reports; none had an error
or external timeout. They include expected node-limit exits for the deliberately
nonconvergent model. Each transaction probe repeats the 1,000 finite-semantics
covering comparisons. Final isolated source/probe files matched the main files
byte-for-byte. All 88 matching sequential verifier cases retained the previous
verdict and exit status; the three added executions are mode-selection probes.

Reports: `.local/selection-sequential.json`, `.local/selection-parallel.json`, and
`.local/selection-z3.json`. No commits or main configuration changes were made.
The assumptions and exclusions in README.md still apply. The earlier REVIEW.md
and RESULTS.md record spec r1 evidence, not this amendment's independent review.
