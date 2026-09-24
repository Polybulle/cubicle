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

  let neutral = Types.neutral_name

  let reverse_call caller caller_args callee_args call =
    let subst = (List.combine call.tc_args callee_args) in
    let subst = LL.filter_map subst ~f:(function
        | None, _ -> None
        | Some var, arg -> Some (var, arg)) in
    let args = List.map (fun var -> List.assoc_opt var subst) caller_args in
    {call with tc_name = caller; tc_args = args}


  let trans_hashtbl f =
    let h = Hstring.H.create (List.length sys) in
    List.iter (fun x -> Hstring.H.add h x.tr_info.tr_name (f x)) sys;
    h


  let nodes = trans_hashtbl (fun t -> t)

  let transition_named s = Hstring.H.find nodes s

  let args_named s =
    if Hstring.equal s neutral then []
    else (transition_named s).tr_info.tr_args

  let entrypoints = LL.filter_map sys ~f:(fun {tr_info = tr; _} ->
      if tr.tr_is_triggered then None
      else Some {tc_name = tr.tr_name;
                 tc_args = List.map (fun _ -> None) tr.tr_args;
                 tc_loc = tr.tr_loc})

  let nexts =
    let nexts = trans_hashtbl (fun {tr_info = tr} ->
        if tr.tr_may_yield then
          tr.tr_nexts @ [{tc_name = neutral; tc_args = []; tc_loc = tr.tr_loc}]
        else tr.tr_nexts) in
    Hstring.H.add nexts neutral entrypoints;
    nexts

  let formal_after name : Ast.transition_call list =
    Hstring.H.find nexts name

  let prevs =
    let prevs = trans_hashtbl (fun t -> ref []) in
    Hstring.H.add prevs neutral (ref []);
    let names = List.map (fun t -> t.tr_info.tr_name) sys @ [neutral] in
    LL.iter names ~f:(fun caller ->
        let caller_args = args_named caller in
        let callees = formal_after caller in
        LL.iter callees ~f:(fun call ->
            let callee_args = args_named call.tc_name in
            let calls = Hstring.H.find prevs call.tc_name in
            calls := reverse_call caller caller_args callee_args call :: !calls));
    Hstring.H.iter (fun _ calls -> calls := List.rev !calls) prevs;
    prevs

  let formal_before name : Ast.transition_call list =
    !(Hstring.H.find prevs name)


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
    let tc_args = List.map (Option.map (Variable.subst subst)) next.tc_args in
    let named_tc_args = List.filter_map (fun x -> x) tc_args in
    let available =
      List.filter (fun v -> not (Hstring.list_mem v named_tc_args)) scope in
    let n_blanks = List.length tc_args - List.length named_tc_args in
    let fresh_vars = Variable.extra_procs scope (Variable.give_procs n_blanks) in
    let finalize procs : event = {evt_trans = next.tc_name; evt_args = procs} in
    resolve finalize available fresh_vars [] tc_args

  let parent_calls_of (n : Node.t) =
    let Before evt = n.state in
    let parents = formal_before evt.evt_trans in
    let subst = Variable.build_subst (args_named evt.evt_trans) evt.evt_args in
    List.concat_map (fill_call n.cube.Cube.vars subst n) parents

  let should_check_safety (c : node_cube) =
    let Before evt = c.state in
    Hstring.equal evt.evt_trans neutral

  let child_calls_of procs evt =
    let sigma = Variable.build_subst (args_named evt.evt_trans) evt.evt_args in
    let rec fill available args = function
      | [] -> [List.rev args]
      | Some p :: rest -> fill available (p :: args) rest
      | None :: rest ->
         List.concat_map (fun (p, others) -> fill others (p :: args) rest)
           (split_like_a_set available) in
    List.concat_map (fun call ->
      let args = List.map (Option.map (Variable.subst sigma)) call.tc_args in
      let named = List.filter_map (fun p -> p) args in
      let available = List.filter (fun p -> not (Hstring.list_mem p named)) procs in
      List.map (fun args -> {evt_trans = call.tc_name; evt_args = args})
        (fill available [] args)) (formal_after evt.evt_trans)

  let should_check_fixpoint  = should_check_safety

  let transition_for_event e = transition_named e.evt_trans

  let it = {
    parent_calls_of;
    child_calls_of;
    should_check_safety;
    should_check_fixpoint;
    transition_for_event
  }

end
