open Ast

let require b message = if not b then failwith message

let () =
  let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
  require (Transaction.is_ordinary sys.t_trans) "ordinary classification";
  require (Transaction.is_ordinary []) "empty ordinary system";
  let tr = List.hd sys.t_trans in
  List.iter (fun info ->
    require (not (Transaction.is_ordinary [{tr with tr_info = info}]))
      "transaction annotation classified as ordinary")
    [{tr.tr_info with tr_is_triggered = true};
     {tr.tr_info with tr_may_yield = false};
     {tr.tr_info with tr_nexts = [{tc_name = tr.tr_info.tr_name;
                                 tc_args = List.map (fun _ -> None) tr.tr_info.tr_args;
                                 tc_loc = tr.tr_info.tr_loc}]}];
  let root = List.hd sys.t_unsafe in
  if Options.tx_bwd then begin
    let dispatched = ref false in
    let cfg = {sys.cfg with parent_calls_of = (fun _ -> dispatched := true; [])} in
    let evt = {evt_trans = tr.tr_info.tr_name;
               evt_args = Variable.give_procs (List.length tr.tr_info.tr_args)} in
    ignore (Pre.pre_image {sys with cfg} {root with state = Before evt; kind = Node});
    require !dispatched "located input bypassed CFG dispatch"
  end;
  let forbid _ = failwith "ordinary predecessor dispatched through CFG" in
  let cfg = {sys.cfg with final_calls = forbid; parent_calls_of = forbid} in
  let ls, post = Pre.pre_image {sys with cfg} root in
  let nodes = ls @ post in
  require (nodes <> []) "empty ordinary pre-image";
  List.iter (fun n ->
    require (n.state = Node.neutral_pos) "ordinary predecessor retained control";
    require (n.depth = 1 && List.length n.from = 1) "ordinary trace depth";
    let _, _, parent = List.hd n.from in
    require (parent == root) "ordinary trace ancestor changed";
    let canonical = {n with cube = Cube.normal_form n.cube} in
    require (Node.normalize canonical == canonical) "canonical ordinary node copied";
    let info, args, _ = List.hd n.from in
    Format.printf "PRE %a(%a): %a@." Hstring.print info.tr_name
      Variable.print_vars args Cube.print canonical.cube) nodes;
  print_endline "PASS ordinary predecessor dispatch and neutral nodes"
