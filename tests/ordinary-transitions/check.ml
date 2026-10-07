open Ast

let require b message = if not b then failwith message

let () =
  let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
  let root = List.hd sys.t_unsafe in
  let ls, post = Pre.pre_image sys root in
  let nodes = ls @ post in
  require (nodes <> []) "empty ordinary pre-image";
  List.iter (fun n ->
    require (n.state = Node.neutral_pos) "ordinary predecessor retained control";
    require (n.depth = 1 && List.length n.from = 1) "ordinary trace depth";
    let _, _, parent = List.hd n.from in
    require (parent == root) "ordinary trace ancestor changed";
    let canonical = {n with cube = Cube.normal_form n.cube} in
    require (Node.normalize canonical == canonical) "canonical ordinary node copied") nodes;
  print_endline "PASS ordinary neutral predecessor identity and history"
