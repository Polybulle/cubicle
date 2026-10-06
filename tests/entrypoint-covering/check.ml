open Ast
open Types

let require b message = if not b then failwith message

let () =
  let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
  let root = List.hd sys.t_unsafe in
  let p = List.hd Variable.procs in
  let at name = Node.create root.cube
      ~pos:(Before {evt_trans = Hstring.make name; evt_args = [p]}) in
  require (sys.cfg.should_check_fixpoint root) "boundary covering disabled";
  List.iter (fun name ->
    require (not (sys.cfg.should_check_fixpoint (at name)))
      ("entry-only location still checked: " ^ name)) ["plain"; "enter"];
  List.iter (fun name ->
    require (sys.cfg.should_check_fixpoint (at name))
      ("internal covering disabled: " ^ name)) ["inner"; "recursive_entry"];
  (* Duplicate entry nodes must reach the boundary rather than being covered here. *)
  let expanded = ref 0 in
  let cfg = {sys.cfg with parent_calls_of = (fun _ -> incr expanded; [])} in
  let n = at "plain" in
  let inv = Node.create root.cube ~pos:n.state ~kind:Node in
  let result = Bwd.Selected.search {sys with cfg; t_unsafe = [n]; t_invs = [inv]} in
  (match result with Bwd.Safe _ -> () | Bwd.Unsafe _ -> failwith "unexpected UNSAFE");
  require (!expanded = 1) "scheduler covered an entry-only node";
  Printf.printf "PASS entrypoint covering policy and scheduler\n%!"
