open Ast
open Types
open Forward

let rec compare_list cmp xs ys = match xs, ys with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
     let c = cmp x y in
     if c = 0 then compare_list cmp xs ys else c

let compare_udnfs a b =
  let normalize = List.map (List.sort SAtom.compare) in
  let cmp = compare_list SAtom.compare in
  compare_list cmp (List.sort cmp (normalize a)) (List.sort cmp (normalize b))

let compare_instance a b =
  let c = SAtom.compare a.i_reqs b.i_reqs in
  if c <> 0 then c else
  let c = compare_udnfs a.i_udnfs b.i_udnfs in
  if c <> 0 then c else
  let c = SAtom.compare a.i_actions b.i_actions in
  if c <> 0 then c else Term.Set.compare a.i_touched_terms b.i_touched_terms

let () =
  try
    let system = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let p = List.nth Variable.procs 0 and q = List.nth Variable.procs 1 in
    let access p = Access (Hstring.make "A", [p]) in
    let touched = {i_reqs = SAtom.empty; i_udnfs = []; i_actions = SAtom.empty;
                   i_touched_terms = Term.Set.singleton (access p)} in
    let renamed = subst_inst_transition [p, q; q, p] touched in
    if not (Term.Set.equal renamed.i_touched_terms (Term.Set.singleton (access q))) then
      failwith "substitution did not rename touched terms";
    let checked = ref 0 in
    List.iter (fun n ->
      let procs = Variable.give_procs n in
      List.iter (fun tr ->
        if List.length tr.tr_info.tr_args <= n then begin
          let args = List.map snd (Variable.build_subst tr.tr_info.tr_args procs) in
          let generic = List.hd (instantiate_transitions procs args [tr]) in
          let direct = instantiate_transitions procs procs [tr] in
          let renamed = List.map (fun sigma ->
            let instance = subst_inst_transition sigma generic in
            let inverse = List.map (fun (x, y) -> y, x) sigma in
            if compare_instance generic (subst_inst_transition inverse instance) <> 0 then
              failwith "compiled substitution round trip changed a field";
            incr checked;
            instance) (Variable.all_permutations procs procs) in
          let direct = List.sort_uniq compare_instance direct in
          let renamed = List.sort_uniq compare_instance renamed in
          if compare_list compare_instance direct renamed <> 0 then
            failwith (Printf.sprintf "compiled instances differ: %s with %d processes"
                        (Hstring.view tr.tr_info.tr_name) n)
        end) system.t_trans) [1; 2; 3; 4];
    Printf.printf "PASS compiled substitution: %d permutations\n%!" !checked
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
