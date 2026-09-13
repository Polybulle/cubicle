open Ast
open Types
module LL = ListLabels


module type System = sig
  val it : transition list
end

module type Cfg = sig
  val it : cfg
end

module CFG_of (S : System) : Cfg = struct

  let sys = S.it

  let reverse_call caller callee call =
    let subst = (List.combine call.tc_args callee.tr_args) in
    let subst = LL.filter_map subst ~f:(function
        | None, _ -> None
        | Some var, arg -> Some (var, arg)) in
    let args = List.map (fun var -> List.assoc_opt var subst) caller.tr_args in
    {call with tc_name = caller.tr_name; tc_args = args}


  let trans_hashtbl f =
    let h = Hstring.H.create (List.length sys) in
    List.iter (fun x -> Hstring.H.add h x.tr_info.tr_name (f x)) sys;
    h


  let nodes = trans_hashtbl (fun t -> t)

  let transition_named s = Hstring.H.find nodes s

  let entrypoints = LL.filter_map sys ~f:(fun {tr_info = tr; _} ->
      if tr.tr_is_triggered then None
      else Some {tc_name = tr.tr_name;
                 tc_args = List.map (fun _ -> None) tr.tr_args;
                 tc_loc = tr.tr_loc})

  let yielders = LL.filter_map sys ~f:(fun {tr_info = tr; _} ->
      if not tr.tr_may_yield then None
      else Some {tc_name = tr.tr_name;
                 tc_args = List.map (fun _ -> None) tr.tr_args;
                 tc_loc = tr.tr_loc})

  let nexts = trans_hashtbl (fun {tr_info = tr} ->
      if tr.tr_may_yield then tr.tr_nexts @ entrypoints else tr.tr_nexts)

  let formal_after (t : transition) : Ast.transition_call list =
    Hstring.H.find nexts t.tr_info.tr_name

  let prevs =
    let prevs = trans_hashtbl (fun t -> ref []) in
    LL.iter sys ~f:(fun t ->
        let caller = t.tr_info in
        let callees = Hstring.H.find nexts caller.tr_name in
        LL.iter callees ~f:(fun call ->
            let callee = (Hstring.H.find nodes call.tc_name).tr_info in
            let calls = Hstring.H.find prevs call.tc_name in
            calls := reverse_call caller callee call :: !calls));
    Hstring.H.iter (fun _ calls -> calls := List.rev !calls) prevs;
    prevs

  let formal_before (t : transition) : Ast.transition_call list =
    !(Hstring.H.find prevs t.tr_info.tr_name)


  let fresh_process_vars scope n =
    let last = List.fold_left (fun last v -> max last (Variable.number v)) 0 scope in
    (* Reserve through the last index: the current scope need not be contiguous. *)
    Variable.extra_procs (Variable.give_procs last) (Variable.give_procs n)

  let split_like_a_set l =
    List.map (fun x -> (x, List.filter (fun y -> not (Hstring.equal x y)) l)) l

  let rec resolve k available fresh_vars acc todo = match todo with
    | [] -> [k (List.rev acc)]
    | Some var :: rest -> resolve k available fresh_vars (var :: acc) rest
    | None :: rest ->
      let resolved_without = List.concat_map
          (fun (var,others) -> resolve k others fresh_vars (var :: acc) rest)
          (split_like_a_set available) in
      let var, fresh_vars = List.hd fresh_vars, List.tl fresh_vars in
      let resolved_with = resolve k available fresh_vars (var :: acc) rest in
      resolved_without @ resolved_with

  let fill_call scope subst (cube : Node.t) (next : Ast.transition_call) =
    let trans = Hstring.H.find nodes next.tc_name in
    let tc_args = List.map (Option.map (Variable.subst subst)) next.tc_args in
    let named_tc_args = List.filter_map (fun x -> x) tc_args in
    let available =
      List.filter (fun v -> not (Hstring.list_mem v named_tc_args)) scope in
    let underscore_count = List.length tc_args - List.length named_tc_args in
    let fresh_vars = fresh_process_vars scope underscore_count in
    let finalize procs : event = {evt_trans = trans; evt_args = procs} in
    resolve finalize available fresh_vars [] tc_args

  let parent_calls_of (n : Node.t) =
    match n.from with
    | [] ->
      List.concat_map (fill_call n.cube.Cube.vars [] n) yielders
    | (tri, args, _) :: _ ->
      let trans = transition_named tri.tr_name in
      let parents = formal_before trans in
      let subst = Variable.build_subst trans.tr_info.tr_args args in
      let in_scope = List.sort_uniq compare (n.cube.Cube.vars @ args) in
      List.concat_map (fill_call in_scope subst n) parents

  let should_check_safety = failwith "not implemented"
  let should_check_fixpoint = failwith "not implemented"

  let it = {
    parent_calls_of;
    should_check_safety;
    should_check_fixpoint
  }

end
