exception Cycle of int list

open Ast

type node = transition_info

type edge = transition_call

(** Type of paths through triggered transitions. *)
(**  Parameter [tr] is the transition info type *)
type 'tr path =
  | Tcp_one of 'tr
  | Tcp_step of 'tr * Ast.transition_call * ('tr path)

val print_path : ('t -> transition_info) -> Format.formatter -> 't path -> unit

val path_map : ('a -> 'b) -> 'a path -> 'b path

val path_rev : 'a path -> 'a path

module type DAG = sig
  val nodes : node array
  val is_input : node -> bool
  val is_output : node -> bool
  val edges_from : node -> edge list
  val dest_node : edge -> node
end

module type Algos = sig
  val is_acyclic : bool
  val paths : node path list
end

(* Raises [Cycle] if [G] contains a cycle. *)
(* The exception then contains the indices of the nodes making the cycle *)
module Make (G : DAG) : Algos
