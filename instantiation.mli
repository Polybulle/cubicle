(**************************************************************************)
(*                                                                        *)
(*                              Cubicle                                   *)
(*                                                                        *)
(*                       Copyright (C) 2011-2014                          *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                       Universite Paris-Sud 11                          *)
(*                                                                        *)
(*                                                                        *)
(*  This file is distributed under the terms of the Apache Software       *)
(*  License version 2.0                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Exhaustive Instantiation features *)

(** Because the SMT solver only takes as input ground formulas, we need to do
    the instantiation of quantifiers inside the model checker. The simplest
    (and complete) way to do this is to saturate exhaustively with the process
    varialbles (skolems). *)

val relevant : of_node:Node.t -> to_node:Node.t -> Variable.subst list
(** [relevant ~of_node:a ~to_node:b] returns substitutions for the quantifiers
    of the cover [a] for the test
    [exists i1,... b => exists z1,... a]. Eliminates trivial useless
    (because they make the goal inconsistent) instantiations with simple
    checks.

    In backward transaction mode, both nodes must be normalized, different
    locations yield no substitutions and transition arguments are mapped to one
    another.

    The ordinary relevance filter is quadratic in the size of the largest
    contiguous subset of [a] and [b] of atoms with terms of the same type.
 *)

val exhaustive : of_node:Node.t -> to_node:Node.t -> Variable.subst list
(** Same contract as {!relevant}, without the data-based relevance filter. *)

val relevant_unnorm : of_node:Node.t -> to_node:Node.t -> Variable.subst list
(** Like {!relevant}, but accepts unnormalized covers. The goal must still be normalized. *)
