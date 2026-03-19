exception Cycle of int list

open Ast

type node = transition_info

type edge = transition_call

(** Type of paths through triggered transitions. *)
(**  Parameter [tr] is the transition info type *)
type path = node * ( (node * edge * node) list )

val print_path : Format.formatter -> path -> unit

val debug_paths : path list -> unit

val path_rev : path -> path

module type DAG = sig
  val nodes : node array
  val is_input : node -> bool
  val is_output : node -> bool
  val edges_from : node -> edge list
  val dest_node : edge -> node
end

module type Algos = sig
  val is_acyclic : bool
  val paths : path list
end

(* Raises [Cycle] if [G] contains a cycle. *)
(* The exception then contains the indices of the nodes making the cycle *)
module Make (G : DAG) : Algos
