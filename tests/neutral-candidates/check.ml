(* Step 8 contracts, linked against the production modules. *)
open Ast
open Types

let require b message = if not b then failwith message
let internal = Before {evt_trans = Hstring.make "enter"; evt_args = []}
let candidate pos cube = Node.create ~pos ~kind:Approx cube

module Oracle = struct
  let calls = ref 0
  let init _ = ()
  let first_good_candidate _ = incr calls; None
end
module Candidates = Approx.Make (Oracle)

let check sys =
  let root = List.hd sys.t_unsafe in
  let inv = List.hd sys.t_invs in
  require (inv.kind = Inv) "supplied invariant lost trusted kind";
  require (inv.state = (if Options.tx_bwd then Node.neutral_pos else Node.dummy_pos))
    "supplied invariant has wrong position";
  require (Hashtbl.fold (fun _ i acc -> acc || i.init_invs <> [])
             sys.t_init_instances false) "lost initial invariant assumptions";
  let node = Node.create ~pos:internal root.cube in
  ignore (Candidates.good node);
  require (!Oracle.calls = (if Options.tx_bwd then 0 else 1))
    "internal approximation reached oracle in transaction mode";
  let c = candidate internal root.cube in
  (* No transitions: an admitted candidate survives in the returned list. *)
  let sys = {sys with cfg = {sys.cfg with parent_calls_of = (fun _ -> [])}} in
  let retained = match Bwd.Selected.search ~candidates:[c] sys with
    | Bwd.Safe (_, candidates) -> candidates
    | Bwd.Unsafe _ -> failwith "unexpected unsafe admission check" in
  require (List.length retained = (if Options.tx_bwd then 0 else 1))
    "candidate admission did not respect transaction mode";
  if Options.tx_bwd then begin
    let expanded = ref false in
    let obligation = Node.create ~pos:internal inv.cube in
    let cfg = {sys.cfg with parent_calls_of = (fun n ->
      if n.tag = obligation.tag then expanded := true;
      [])} in
    ignore (Bwd.Selected.search {sys with cfg; t_unsafe = [obligation]});
    require !expanded "supplied invariant covered an internal obligation";
    let bad = candidate Node.neutral_pos inv.cube in
    require (Approx.remove_bad_candidates sys bad [c; bad] = [])
      "restart retained an internal or failed candidate";
    let same = candidate Node.neutral_pos root.cube in
    let raised = try
      ignore (Approx.remove_bad_candidates sys same [same]); false
    with Safety.Unsafe _ -> true in
    require raised "reachable original formula was treated only as an approximation";
    require (Node.origin same == same && (Node.origin same).kind = Approx)
      "approximation origin lost";
    require ((Node.origin root).kind = Orig) "original origin lost";
    let reachable = candidate Node.neutral_pos (Cube.create [] SAtom.empty) in
    let survivor = candidate Node.neutral_pos root.cube in
    survivor.deleted <- true;
    let faulty = match Bwd.Selected.search ~candidates:[reachable] sys with
      | Bwd.Unsafe (faulty, _) -> faulty
      | Bwd.Safe _ -> failwith "reachable approximation was accepted" in
    require ((Node.origin faulty).kind = Approx)
      "reachable approximation became an original counterexample";
    let kept = Approx.remove_bad_candidates sys faulty [reachable; survivor; c] in
    require (List.length kept = 1 && List.hd kept == survivor)
      "restart lost the unreachable surviving candidate";
    require (not survivor.deleted) "restart did not reset survivor deletion";
    match Bwd.Selected.search ~candidates:kept sys with
    | Bwd.Safe _ -> ()
    | Bwd.Unsafe _ -> failwith "failed approximation polluted restarted search"
  end

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    check sys;
    Printf.printf "PASS neutral candidate contracts\n%!"
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
