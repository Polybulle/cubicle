# Transaction-Guided Parameterized Model-Checking in Cubicle

## Thesis

During the conception phase of software, one builds iteratively and from good to
worse: first, the happy paths are written, and only then are errors and failures
taken into account, at an increasingly detailed and pessimistic level. When that
software is based on some notion of protocol and transactions, we first consider
what the protocol should be, and only then what can cause its transactions to
fail. We wish to develop model checking frameworks that enable this iterative
process, by letting users decide where they stand on the spectrum between the
idealized models of early conception and the pessimistic, more realistic models
of late-stage verification.

We build such a framework over the MCMT framework, as implemented in the Cubicle
model checker, by introducing explicit protocols and transactions with rollback
into models. We manage to do so without incurring safety or performance loss,
while enabling more elegant and more performant models than by merely encoding
protocol-level information into ordinary MCMT transitions.

## 1. Introduction

### Problem

Parameterized systems must be verified for an unbounded number of processes,
which implies an infinite state-space. Tools such as Cubicle model these systems
as transition systems over arrays indexed by process identifiers, then prove
safety by combining SMT-based backward reachability with finite-instance
exploration along the MCMT framework.

The modeling language exposes control flow as many fine-grained transitions that
fire when a guard is validated by some subset of the system. This creates a
mismatch: the system designer thinks in protocols, transactions, and rollback
points, while the verifier explores individual rule applications and therefore
many partial flows and interleavings thereof. This leads not only to lost
productivity but also puts the onus on SMT solvers to rediscover control flow
restrictions that were already clear at the protocol level.

### Approach

We extend Cubicle with annotations that let the user expose transaction
structure explicitly while keeping the underlying MCMT discipline:

- `triggered transition` marks a transition as a continuation step.
- `triggers t(...) or u(...)` declares possible continuation transitions.
- `yields` marks transaction boundaries.

The type checker validates these annotations and elaborates them into
transaction paths. These paths are then consumed by forward and backward search.
Within a path, the same process identifiers are reused consistently along steps,
as opposed to being re-drawn arbitrarily. By adding or removing links between
transitions, the same model can move progressively from idealized protocol
toward more pessimistic and realisitic model.

### Results and Contributions

1. A conservative extension of Cubicle syntax for explicit transaction
   annotations.
2. A static path-extraction pass based on an acyclic trigger graph and
   permutation expansion for transaction parts.
3. A forward-search implementation that composes transaction paths while still
   recording intermediate states for candidate rejection.
4. A backward-search implementation that carries intermediate transaction state
   through pre-image computation.
5. An API allowing progressive moves between transaction-based and purely
   rule-based models.
6. An evaluation centered on FLASH showing a 3.4x reduction in forward states
   and about a 4x reduction in runtime under the recorded BRAB configuration.

## 2. Background: Cubicle and BRAB

- Cubicle
- MCMT
- SMT
- BRAB

## 3. Transaction Annotation Language

The extension adds a small amount of protocol-level control-flow structure to
Cubicle models:

- `triggered transition t (...)` marks `t` as a continuation step.
- `triggers t1(...) or t2(...)` lists possible continuation steps.
- `yields` marks transaction boundaries.
- `part name { ... }` splits one transition into atomic transaction parts.

The intended modeling style is incremental. Early versions can group the happy
path of a protocol into a single transaction. Later versions can introduce
failure branches, yields, and break a transaction into smaller ones when the
verification question demands a more pessimistic model.

- TODO 
  - [Introduce a simple example we'll reuse consistently]
  - [Explain the backtracking semantics]
  - [Explain path generation during typing]
  - [Explain the gamut of choice the user has in forward/backward exploration
and different safety notions they imply]


## 4. Backward Transaction Search

During backward search, pre-images of transitions are replaced with pre-images
of transaction paths. This is where rollback semantics are most natural: going
backward from a bad state, the search carries the future of the transaction that
would have had to occur for that state to be reached.

- The set of all possible assignments of process IDs to the variables of the
  path is computed, as one would for a single transition.
- The pre-image of the last step of the path is taken.
- Safety is checked, if relevant.
- The remainder of the path plus the chosen parameters are stored with the node.

Notes:

- No fixpoint check is performed for nodes in the middle of paths.
- Ordinary pre-image computation is reused for each step, so the core MCMT
  machinery is preserved.
- Unnormalized cubes are used while following a path for performance reasons.

## 5. Forward Transaction Search

Forward search uses transaction paths to avoid exploring arbitrary partial
protocol fragments as first-class finite instances. The goal is not to hide
intermediate states, but to let finite exploration follow the same transaction
structure that the modeler wrote down.

[Use `examples/flash_nodata_tract.cub` here]

```
transition pi_Remote_Get (src) { ... }
triggers ni_Local_Get_Nak1(src)
      or ni_Local_Get_Nak(src)
      or ni_Local_Get_Get(src)
      or ni_Local_Get_Put1(src)
```

| Configuration | Forward states | Backward nodes | Time | Result |
|---|---:|---:|---:|---|
| BRAB 2, `-tract ignore` | 191,649 | 37 | 2.5s | SAFE |
| BRAB 2, `-tract fwd` | 56,099 | 37 | 0.6s | SAFE |
| BRAB 2, `-tract all` | 56,099 | 31 | 0.7s | SAFE |

## 6. Semantics via Compilation

The extension can be compiled to Cubicle by explicitly encoding the underlying
finite automaton the paths represents:
- create a new enumerated type ST with one value "dispatch"
- add variable state of type ST
- For each path X with arguments x_1,...x_n:
  - create variable path_X_arg_1,...path_X_arg_n of type proc
  - create a transition dispatch_X with args x_1,...,x_n
  - add guard state = dispatch
  - add action {state := path_X_step_1; path_X_arg_1 := x_1; ... path_X_arg_n := x_n}
- For each step Y of each path X
  - Add an value "path_X_step_Y" to ST
  - instantiate the transition to replace its arguments y_1,...y_m with the relevant "path_X_arg_?"
  - add a guard on state = path_X_step_Y
  - add an action state := path_X_step_(Y+1) or state := dispatch for the last step 

This is correct since the normal-form on loop-free finite-automata is a
disjunction of sequences. Compared to our native setup, the SMT solver not only
has to deal with the added finite state but more importantly with the
path_X_arg_N variables, which are themselves array indices, and the relation
between the current state and which argument variable is relevant. This is a
weak point of MCMT-backed model-checkers, that we manage to completely
side-step.

[Provide a comparison over the example.]

## 7. TODO Evaluation Plan

Does `-tract fwd` reduce irrelevant finite exploration? (yes, see FLASH)
Does `-tract bwd` reduce backward proof search? (yes, solver calls)
How much overhead is introduced by transaction-path compilation? (none)

Benchmarks to consider:

- `examples/flash_nodata.cub`
- `examples/german_baukus.cub`
- `examples/german_pfs_data.cub`
- `dir_msi.cub`
- `dir_mesi.cub`
- `examples/challenges/flash2_nodata.cub`

## 8. Limitations

- Annotation correctness is the modeler's responsibility.
- Intermediate nodes in paths are under-exploited.
- Trigger graphs are acyclic.
- Current implementation has incompatibilities with some Cubicle features such
  as Murphi output and trace output.

## 9. Conclusion and Future Work

Future work mainly focuses on having many transactions progress simultaneously,
and on using the same annotation language to support a smoother refinement path
from happy-path protocol models to pessimistic verification models.

