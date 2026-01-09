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



open Ast
open Types

(** Symbolic forward exploration *)


module HSA : Hashtbl.S with type key = SAtom.t

module MA : Map.S with type key = Atom.t

(** the type of instantiated transitions *)
type inst_trans =
    {
      i_reqs : SAtom.t;
      i_udnfs : SAtom.t list list;
      i_actions : SAtom.t;
      i_touched_terms : Term.Set.t;
    }

type possible_result =
  | Reach of (SAtom.t * transition_info * Variable.subst * SAtom.t) list 
  | Spurious of trace
  | Unreach

(* val search : Hstring.t list -> t_system -> SAtom.t list *)

val all_var_terms : Variable.t list -> t_system -> Term.Set.t

val search : Hstring.t list -> t_system -> unit HSA.t

val search_stateless : Hstring.t list -> t_system -> (SAtom.t * Term.Set.t) MA.t


(** instantiate transitions with a list of possible parameters *)
val instantiate_transitions : Variable.t list -> Variable.t list ->
  transition list -> inst_trans list

val abstract_others : SAtom.t -> Hstring.t list -> SAtom.t

val reachable_on_trace_from_init :
  t_system -> Node.t -> trace -> possible_result


(** check if the history of a node is spurious *)
val spurious : Node.t -> bool

(** check if an error trace is spurious *)
val spurious_error_trace : t_system -> Node.t -> bool

(** check if an error trace is spurious due to the {b Crash Failure Model } *)
val spurious_due_to_cfm : t_system -> Node.t -> bool

(** Replays the history of a faulty node and returns (possibly) an error
    trace *)
val replay_history :
  t_system -> Node.t ->
  (SAtom.t * transition_info * Variable.subst * SAtom.t) list option

(** check if an error trace is spurious due to the {b Crash Failure Model } *)
val conflicting_from_trace : t_system -> trace -> SAtom.t list

(** put a universal guard in disjunctive normal form *)
val uguard_dnf : 
  Variable.subst ->
  Variable.t list -> Variable.t list ->
  (Variable.t * SAtom.t list) list -> SAtom.t list list



module H = Hstring

(** Renamings and eliminations of renamed terms *)

val prime_term : Types.term -> Types.term (** alpha-renamed copy of a term *)
val prime_satom : Types.SAtom.t -> Types.SAtom.t (** alpha-renamed copy of a cube *)
val unprime_h : Hstring.t -> Hstring.t (** original of a (possibly renamed) variable *)

(** Detect if a variable/term has been renamed *)
val is_prime : string -> bool
val is_prime_term : Types.term -> bool

(** Attempt to eliminate some primed term of finite type constant prop. *)
val elim_prime : Types.SAtom.t -> Types.SAtom.t -> Types.SAtom.t

(** Attempt to eliminate all prime terms of infinite type from a cube through eq and const prop. *)
val elim_prime2 : Types.SAtom.t -> Types.SAtom.t

(** Attempt to eliminate all prime terms of 2nd cube and accumulate into first *)
val elim_prime_type2 : Types.SAtom.t -> Types.SAtom.t -> Types.SAtom.t

(** Sam as above with simplification *)
val wrapper_elim_prime : Types.SAtom.t -> Types.SAtom.t -> Types.SAtom.t



(** [swts_to_ites t cases sigma] compiles sigma(case t of cases) to if-then-elses *)
val swts_to_ites :
  Types.term ->
  (Types.SAtom.t * Types.term) list ->
  Variable.subst ->
  Types.Atom.t

(** converts an update block (of vars) under a subst into an atom. before-vars are primed,
    new-vars are not. *)
val apply_assigns :
  (Hstring.t * Ast.glob_update) list ->
  Variable.subst ->
  Types.SAtom.t * Types.Term.Set.t

(** converts an update block (of arrays) under a subst and actual procs into an
    atom. before-vars are primed, new-vars are not. *)
val apply_updates :
  Ast.update list ->
  Hstring.t list ->
  Variable.subst ->
  Types.SAtom.t * Types.Term.Set.t

(** Given a list of (unmodified) variables and an atom representing an update,
    add a re-assignement encoding the non-update *)
val preserve_terms :
  Types.Term.Set.t -> Types.SAtom.t -> Types.SAtom.t


(** [possible_init _ a b] checks if the requirements [b] are compatible with
    init condition [a]*)
val possible_init :
  'a -> Types.SAtom.t -> Types.SAtom.t -> bool

(** checks that the guard of a transition is compatible with init condition *)
val possible_guard :
  'a ->
  H.t list ->
  Hstring.t list ->
  Variable.subst ->
  Types.SAtom.t ->
  Types.SAtom.t ->
  (Hstring.t * Types.SAtom.t list) list ->
  bool

(** ? *)
val possible_inst_guard :
  'a ->
  'b ->
  Types.SAtom.t ->
  Types.SAtom.t ->
  Types.SAtom.t list list ->
  bool

val missing_args :
  'a list -> Hstring.t list -> Hstring.t list * Hstring.t list

val term_contains_arg : Hstring.t -> Types.term -> bool
val atom_contains_arg : Hstring.t -> Types.Atom.t -> bool

(** Given a forward state and transition, compute the next possible forward states *)
val post_inst :
  Types.SAtom.t ->
  'a ->
  'b ->
  inst_trans ->
  (Types.SAtom.t * Hstring.t list) list

val already_seen :
  Types.SAtom.t -> Hstring.t list -> 'a HSA.t -> bool

(** given a list of transitions and list of state, run forward reach until a
    fixpoint is reached *)
val forward :
  'a ->
  'b ->
  inst_trans list ->
  (HSA.key * Hstring.t list) list ->
  unit HSA.t

(** is an atom unconstrained by a term *)
val var_term_unconstrained : Types.SAtom.t -> Types.term -> bool
val unconstrained_terms : Types.SAtom.t -> Types.Term.Set.t -> Types.Term.Set.t

(** Add to the map, for each atom in the cube, the set of all other atoms (its
    companions) and the set of all global variables is does not constrain *)
val add_compagnions_from_node :
  Types.Term.Set.t ->
  Types.SAtom.t ->
  (Types.SAtom.t * Types.Term.Set.t) MA.t ->
  (Types.SAtom.t * Types.Term.Set.t) MA.t

(** [stateless_forward _ _ trs vars states] computes the reachable cubes
    attainable through instantiated transitions [trs] from states [states]. The
    set [vars] must contain the instantiated global variables of the system. *)
val stateless_forward :
  'a ->
  'b ->
  inst_trans list ->
  Types.Term.Set.t ->
  (Types.SAtom.t * Hstring.t list) list ->
  (Types.SAtom.t * Types.Term.Set.t) MA.t

(** Given a list of process ids [procs] and a system [sys], calling [mkinits
    procs sys] returns the initial condition of the system as a conjonctions
    atom, whose existential variables are replaced by all possible combination
    of processes in [procs] *)
val mkinits :
  Hstring.t list ->
  Ast.t_system ->
  (Types.SAtom.t * Hstring.t list) list

(** [instance_of_transition tr procs others subst] returns the instantiated
    transition [tr] under substitution [subst], where all variables in [others]
    are abstracted (their updates are removed). [procs] must contain all
    instantiated process variables. *)
val instance_of_transition :
  Ast.transition_info ->
  Hstring.t list ->
  Hstring.t list ->
  Variable.subst ->
  inst_trans

val search_only : 'a -> 'b

exception
  Reachable of
    (Types.SAtom.t
    * Ast.transition_info
    * Variable.subst
    * Types.SAtom.t)
    list

val mkinits_up_to :
  Hstring.t list list ->
  Ast.t_system ->
  (Types.SAtom.t * Hstring.t list) list


(** Given a trace [trace], an inital condition [starts] as a list of
    (instantiated) cubes, and a condition to look for [finish], return the
    history of the system that reaches it from a cube in [start]. *)
val possible_trace :
  starts:(Types.SAtom.t * Hstring.t list) list ->
  finish:Ast.node_cube ->
  procs:Hstring.t list ->
  trace:Ast.trace_step list ->
  possible_result


val procs_on_trace :
  ('a * Hstring.t list * Ast.node_cube) list -> Hstring.t list

val reachable_on_all_traces_from_init :
  Ast.t_system ->
  Ast.node_cube ->
  Ast.trace_step list ->
  possible_result

val possible_history : Ast.node_cube -> possible_result


val reachable_on_trace_from_init :
  t_system -> Node.t -> trace -> possible_result


(** check if the history of a node is spurious *)
val spurious : Node.t -> bool

(** check if an error trace is spurious *)
val spurious_error_trace : t_system -> Node.t -> bool

(** check if an error trace is spurious due to the {b Crash Failure Model } *)
val spurious_due_to_cfm : t_system -> Node.t -> bool

(** Replays the history of a faulty node and returns (possibly) an error
    trace *)
val replay_history :
  t_system -> Node.t ->
  (SAtom.t * transition_info * Variable.subst * SAtom.t) list option

(** check if an error trace is spurious due to the {b Crash Failure Model } *)
val conflicting_from_trace : t_system -> trace -> SAtom.t list
