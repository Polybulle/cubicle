open Ast
open Types

let require b message = if not b then failwith message
let pos name args = Before {evt_trans = Hstring.make name; evt_args = args}

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let root = List.hd sys.t_unsafe in
    let p = List.nth Variable.procs 0 and q = List.nth Variable.procs 1 in
    let internal = List.map (fun state -> Node.create ~pos:state root.cube)
        [pos "t" [p]; pos "t" [p]; pos "u" [p]; pos "t" [q]] in
    let inv = Node.create ~pos:Node.neutral_pos ~kind:Inv root.cube in
    let expanded = ref [] in
    let cfg = {sys.cfg with parent_calls_of = (fun n ->
      expanded := n.tag :: !expanded;
      [])} in
    let result = Bwd.Selected.search {sys with cfg; t_unsafe = internal; t_invs = [inv]} in
    (match result with
     | Bwd.Unsafe _ -> failwith "internal obligation was checked as initial"
     | Bwd.Safe (visited, _) ->
        require (List.for_all (fun n -> n.state = Node.neutral_pos) visited)
          "internal obligation stored in cube-only trie");
    require (List.length !expanded = List.length internal &&
             List.for_all (fun n -> List.mem n.tag !expanded) internal)
      "internal obligation was covered instead of expanded";
    require (List.for_all (fun n -> not n.deleted) internal)
      "internal obligation was deleted";
    let visited = Cubetrie.add_node inv Cubetrie.empty in
    require (Fixpoint.FixpointTrie.check root visited <> None)
      "ordinary boundary covering stopped working";
    Printf.printf "PASS boundary-only covering contracts\n%!"
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
