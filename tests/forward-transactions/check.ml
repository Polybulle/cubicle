open Ast
open Types

let name = Hstring.make
let bit value = Elem (name (if value then "One" else "Zero"), Constr)
let decode terms sa =
  List.fold_left (fun (mask, index) term ->
    let has value =
      SAtom.mem (Atom.Comp (term, Eq, bit value)) sa ||
      SAtom.mem (Atom.Comp (bit value, Eq, term)) sa in
    if has true then mask lor (1 lsl index), index + 1
    else if has false then mask, index + 1
    else failwith "symbolic state is not a full Boolean valuation") (0, 0) terms |> fst

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let count = int_of_string (Sys.getenv "TEST_PROCS") in
    let procs = Variable.give_procs count in
    let terms = Elem (name "X", Glob) :: Elem (name "Y", Glob) ::
      List.map (fun p -> Access (name "A", [p])) procs in
    let states = Forward.search procs sys in
    let masks = Forward.HSA.fold (fun sa () masks -> decode terms sa :: masks) states [] in
    List.iter (Printf.printf "SYMBOLIC %d\n") (List.sort_uniq compare masks);
    (* Check the stateless projection uses the same located exploration. *)
    if Options.tx_fwd then begin
      let expected = Forward.HSA.fold (fun sa () map ->
        SAtom.fold (fun atom map -> Forward.MA.add atom () map) sa map)
        states Forward.MA.empty in
      let actual = Forward.search_stateless procs sys in
      if List.map fst (Forward.MA.bindings expected) <>
         List.map fst (Forward.MA.bindings actual) then
        failwith "stateless traversal has different reachable atoms"
    end;
    Enumerative.search procs sys;
    for mask = 0 to (1 lsl List.length terms) - 1 do
      let _, atoms = List.fold_left (fun (i, atoms) term ->
        let value = mask land (1 lsl i) <> 0 in
        i + 1, SAtom.add (Atom.Comp (term, Eq, bit value)) atoms)
        (0, SAtom.empty) terms in
      let node = Node.create ~pos:Node.neutral_pos ~kind:Approx (Cube.create procs atoms) in
      if Enumerative.smallest_to_resist_on_trace [node] = [] then
        Printf.printf "ENUMERATIVE %d\n" mask
    done;
    print_endline "PASS forward probe"
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
