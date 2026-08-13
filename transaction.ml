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
open Atom

(** An edge in a transition call graph: source is caller transition, *)
(** edge is call itself, target is callee *)
type edge = (transition_info * transition_call * transition_info)

(** A raw transaction is an initial transition and a list of edges. *)
(** The initial transition is the source of the first edge, *)
(** and target of edges agree with sources of the following edges. *)
type raw_transaction =
  transition_info * (edge list)

(** A path is a scope of process variables, and a list of transition call. *)
(** Each contains a transition (with its normal formal args) and a list of actual args. *)
(** Actual args are always taken from the path's scope. *)
type path =
  Variable.t list * (transition_info * Variable.t list) list

exception Cycle of Hstring.t list

let print_arg fmt = function
  | None -> Format.pp_print_string fmt "_"
  | Some v -> Variable.print fmt v

let print_raw_transaction fmt (src, p) =
  let open Format in
  let go_one n =
    fprintf fmt "%a(%a)"
      Hstring.print n.tr_name
      Variable.print_vars n.tr_args in
  let rec go_tail e p =
    fprintf fmt " -> %a(%a)"
      Hstring.print e.tc_name
      (pp_print_list ~pp_sep:pp_print_space print_arg) e.tc_args;
    match p with
    | [] -> ()
    | (t,e',_)::p' ->
      go_tail e' p' in
  match p with
  | [] -> go_one src
  | (n,e,_)::p ->
    pp_open_hbox fmt ();
    go_one n;
    go_tail e p;
    pp_close_box fmt ()

let debug_raw_transactions ps =
  if Options.debug || Options.verbose > 0 then begin
    let open Format in
    if List.length ps = 0 then
      printf "Found 0 paths through triggers in the model.@."
    else
      printf "@[<v 2>Found the following %n trigger path(s):@;%a@]@."
        (List.length ps)
        (pp_print_list ~pp_sep:pp_print_cut print_raw_transaction) ps
  end

let trigger_paths s =
  let nodes = Array.of_list s.trans in
  let indices = Hstring.H.create (Array.length nodes) in
  Array.iteri (fun i tr -> Hstring.H.add indices tr.tr_name i) nodes;
  let edges =
    Array.map (fun tr ->
        List.map (fun call -> call, Hstring.H.find indices call.tc_name)
          tr.tr_nexts)
      nodes in
  let is_input i =
    not nodes.(i).tr_is_triggered && nodes.(i).tr_parts = [] in
  let is_output i =
    nodes.(i).tr_may_yield && nodes.(i).tr_parts = [] in

  let exception Found_cycle in
  let marks = Array.make (Array.length nodes) `NotSeen in
  let rec check_acyclic i =
    match marks.(i) with
    | `Done -> ()
    | `Current -> raise Found_cycle
    | `NotSeen ->
      marks.(i) <- `Current;
      List.iter (fun (_, j) -> check_acyclic j) edges.(i);
      marks.(i) <- `Done in
  begin
    try
      for i = 0 to Array.length nodes - 1 do
        check_acyclic i
      done
    with Found_cycle ->
      let involved = ref [] in
      for i = 0 to Array.length nodes - 1 do
        if marks.(i) = `Current then
          involved := nodes.(i).tr_name :: !involved
      done;
      raise (Cycle !involved)
  end;

  let raw_transaction source rev_path =
    nodes.(source),
    List.rev_map (fun (i, call, j) -> nodes.(i), call, nodes.(j)) rev_path in
  let rec collect_paths source i rev_path paths =
    List.fold_left (fun paths (call, j) ->
        let rev_path = (i, call, j) :: rev_path in
        let paths =
          if is_output j then raw_transaction source rev_path :: paths else paths in
        if is_input j && is_output j
        then paths
        else collect_paths source j rev_path paths)
      paths edges.(i) in
  let rec collect_sources i paths =
    if i = Array.length nodes then paths
    else if is_input i then
      let paths = if is_output i then (nodes.(i), []) :: paths else paths in
      collect_sources (i + 1) (collect_paths i i [] paths)
    else
      collect_sources (i + 1) paths in
  let ps = collect_sources 0 [] in
  debug_raw_transactions ps;
  ps

let rec all_permutations = function
  | [] -> [[]]
  | lst ->
    let rec pick_one acc = function
      | [] -> []
      | x :: xs ->
        let rest = List.rev_append acc xs in
        let perms = List.map (fun p -> x :: p) (all_permutations rest) in
        perms @ pick_one (x :: acc) xs
    in
    pick_one [] lst

let transition_from_part tr part =
  let name =
    let open Hstring in
    make @@ (view tr.tr_name) ^ "." ^ (view part.tract_part_name) in
  {tr_name = name;
   tr_args = tr.tr_args;
   tr_reqs = SAtom.empty;
   tr_ureq = [];
   tr_lets = part.tract_lets;
   tr_assigns = part.tract_assigns;
   tr_upds = part.tract_upds;
   tr_nondets = part.tract_nondets;
   tr_loc = part.tract_part_loc;
   tr_is_triggered = true;
   tr_may_yield = false;
   tr_nexts = [];
   tr_parts = []}

let transitions_of_transaction tr =
  let with_check t =
    { t with tr_name = tr.tr_name;
      tr_args = tr.tr_args;
      tr_reqs = tr.tr_reqs;
      tr_ureq = tr.tr_ureq;
      tr_is_triggered = false } in
  let trs = List.map (transition_from_part tr) tr.tr_parts in
  let trs' = ref trs in
  let[@warning "-8"] add_head ((t,args)::rest) =
    let t' = with_check t in
    trs' := t' :: !trs';
    (t',args)::rest in
  let ps =
    let calls = List.map (fun t -> (t, t.tr_args)) trs in
    let ps = all_permutations calls in
    List.map (fun p -> (tr.tr_args, add_head p)) ps in
  (!trs', ps)

let transaction_paths s =
  let tracts = List.filter (fun tr -> tr.tr_parts <> []) s.trans in
  let base = {s with trans = List.filter (fun tr -> tr.tr_parts = []) s.trans} in
  let (s,ps) = ListLabels.fold_left tracts ~init:(base,[]) ~f:(fun (s,ps) tr ->
      let trs, ps' = transitions_of_transaction tr in
      {s with trans = trs @ s.trans}, ps @ ps') in
  if Options.verbose > 0 then begin
    let open Format in
    let print_tcall fmt (tr, args) =
      fprintf fmt "%a(%a) ->@ "
        Hstring.print tr.tr_name
        Variable.print_vars args in
    let print_path fmt (_, calls) =
      fprintf fmt "@[%aEND@]" (pp_print_list print_tcall) calls in
    if List.length ps = 0 then
      printf "Found 0 paths through model transactions.@."
    else begin
      printf "@[<v 2>Found following %n paths through model transactions.@;%a@]@."
        (List.length ps)
        (pp_print_list ~pp_sep:pp_print_cut print_path) ps
    end
  end;
  (s,ps)

type resolved_call = {
  args : Variable.t list;
  in_scope : Variable.t list;
}

let fresh_process_var () =
  let v = Variable.gen_var () in
  Smt.Symbol.declare v [] Smt.Type.type_proc;
  v

let resolve_call subst caller in_scope {tc_args; _} =
  let caller_vars = List.map (Variable.subst subst) caller.tr_args in
  let available =
    List.filter (fun v -> not (Hstring.list_mem v caller_vars)) in_scope in
  let fresh_vars =
    List.init
      (List.fold_left (fun n -> function None -> n + 1 | Some _ -> n) 0 tc_args)
      (fun _ -> fresh_process_var ()) in
  let rec resolve available fresh_vars fresh_args rev_args = function
    | [] ->
      let fresh_args = List.rev fresh_args in
      [{args = List.rev rev_args; in_scope = in_scope @ fresh_args}]
    | Some var :: rest ->
      resolve available fresh_vars fresh_args
        (Variable.subst subst var :: rev_args) rest
    | None :: rest ->
      let resolved_without =
        List.concat_map (fun var ->
            let available =
              List.filter (fun v -> not (Hstring.equal var v)) available in
            resolve available fresh_vars fresh_args (var :: rev_args) rest)
          available in
      let var, fresh_vars = List.hd fresh_vars, List.tl fresh_vars in
      let resolved_with =
        resolve available fresh_vars (var :: fresh_args) (var :: rev_args) rest in
      resolved_without @ resolved_with in
  resolve available fresh_vars [] [] tc_args

let path_to_futures (src,p) =
  let rec aux subst in_scope rev_calls = function
    | [] -> [in_scope, List.rev rev_calls]
    | (caller, call, callee) :: rest ->
      List.concat_map (fun {args; in_scope} ->
          let subst = Variable.build_subst callee.tr_args args in
          aux subst in_scope ((callee, args) :: rev_calls) rest)
        (resolve_call subst caller in_scope call) in
  aux [] src.tr_args [src, src.tr_args] p

let expand_trigger_path ps' (globs, calls) =
  let perms_for tri =
    List.filter (fun (_, perm_calls) ->
        match perm_calls with
        | [] -> false
        | (head, _) :: _ -> head.tr_name = tri.tr_name)
      ps' in
  let rec aux = function
    | [] -> [[]]
    | (tri, args) :: rest ->
      let rest_exps = aux rest in
      if tri.tr_parts = [] then
        List.map (fun r -> (tri, args) :: r) rest_exps
      else
        let subst = Variable.build_subst tri.tr_args args in
        List.concat_map (fun (_, perm_calls) ->
            let substituted = List.map
                (fun (t, pargs) -> (t, List.map (Variable.subst subst) pargs))
                perm_calls in
            List.map (fun r -> substituted @ r) rest_exps)
          (perms_for tri)
  in
  List.map (fun expanded -> (globs, expanded)) (aux calls)

let paths s =
  let ps = List.concat_map path_to_futures (trigger_paths s) in
  let s, ps' = transaction_paths s in
  let expanded_ps = List.concat_map (expand_trigger_path ps') ps in
  s, expanded_ps @ ps'

let finalize_future trs (globs, calls) =
  match calls with
  | [] -> failwith "Invariant break: empty path"
  | (args,_)::_ ->
    let find tri = List.find (fun t -> t.tr_info == tri) trs in
    (globs, List.rev_map (fun (tri, args) -> (find tri, args)) calls)

let finalize trs ps = List.map (finalize_future trs) ps
