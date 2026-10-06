open Ast
open Types

let require b message = if not b then failwith message
let name = Hstring.make
let neutral e = Hstring.equal e.evt_trans neutral_name

let () =
  let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
  let root = List.hd sys.t_unsafe in
  let procs = Variable.give_procs 2 in
  let event s = {evt_trans = name s; evt_args = [List.hd procs]} in
  let at s = Node.create ~pos:(Before (event s)) root.cube in
  List.iter (fun (s, initial, final, parents) ->
    let e = event s in
    require (sys.cfg.is_initial e = initial) (s ^ ": initial");
    require (sys.cfg.is_final e = final) (s ^ ": final");
    require (sys.cfg.has_parents e = parents) (s ^ ": parents");
    require (List.for_all (fun e -> not (neutral e))
      (sys.cfg.parent_calls_of (at s))) "neutral predecessor";
    require (List.for_all (fun e -> not (neutral e))
      (sys.cfg.child_calls_of procs e)) "neutral successor")
    ["plain", true, true, false; "enter", true, false, false;
     "inner", false, true, true; "recursive_entry", true, true, true];
  require (List.for_all sys.cfg.is_initial (sys.cfg.initial_calls procs)) "initial calls";
  require (List.for_all sys.cfg.is_final (sys.cfg.final_calls root)) "final calls";
  require (List.length (sys.cfg.initial_calls procs) = 6) "initial instantiation";
  if Options.tx_bwd then begin
    let target = Node.create ~pos:Node.neutral_pos (Cube.create [] SAtom.empty) in
    let only s =
      let cfg = {sys.cfg with final_calls = (fun _ -> [event s])} in
      let ls, post = Pre.pre_image {sys with cfg} target in
      ls @ post in
    let plain = only "plain" in
    require (plain <> [] && List.for_all (fun n -> n.state = Node.neutral_pos) plain)
      "entry-only event retained control bindings";
    (* Restrict the executable pre-image to the recursive entry, keeping the real CFG. *)
    let cfg = {sys.cfg with final_calls = (fun _ -> [event "recursive_entry"])} in
    let ls, post = Pre.pre_image {sys with cfg} root in
    let nodes = ls @ post in
    let released, located = List.partition (fun n -> n.state = Node.neutral_pos) nodes in
    require (released <> [] && List.length released = List.length located)
      "initial/internal node was not split";
    List.iter2 (fun a b ->
      require (a.cube == b.cube) "release rebuilt or lost cube witnesses";
      require (a.from == b.from && a.depth = b.depth) "release changed history";
      require (sys.cfg.should_check_fixpoint b) "internal covering disabled";
      let parents = sys.cfg.parent_calls_of b in
      require (parents <> [] && List.for_all (fun e -> not (neutral e)) parents)
        "retained branch contains boundary predecessor") released located
  end;
  print_endline "PASS CFG boundaries and split release"
