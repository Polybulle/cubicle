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

(** Elaboration of transaction paths *)

type path

exception Cycle of Hstring.t list

val paths : system -> system * path list
(** Elaborates trigger paths and transaction parts.
    Raises [Cycle] with the transitions involved if the trigger graph is cyclic. *)

val finalize : transition list -> path list ->
  (Variable.t list * transaction_path) list
(** Replaces source transitions in elaborated paths with compiled transitions. *)
