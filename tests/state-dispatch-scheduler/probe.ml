(* Independent r2 contracts, linked only against public baseline interfaces. *)
open Ast
open Types

let require b message = if not b then failwith message
let neutral = function Before e -> Hstring.equal e.evt_trans Types.neutral_name
let event name args = {evt_trans = Hstring.make name; evt_args = args}
let pos name args = Before (event name args)
let all_pre sys n = let a,b = Pre.pre_image sys n in a @ b
let trans sys name = List.find (fun t -> Hstring.equal t.tr_info.tr_name (Hstring.make name)) sys.t_trans
let calls_equal a b = a = b
let single = function [x] -> x | _ -> failwith "expected one predecessor"

let cfg_state sys =
  let vars = Variable.give_procs 4 in
  let p = List.nth vars 1 and q = List.nth vars 3 in
  let cube = Cube.create [] SAtom.empty in
  let n = Node.create ~pos:(pos "relay" [p;q]) cube in
  let expected = [event "enter" [q;p]] in
  List.iter (fun kind ->
    let n = {n with kind} in
    require (calls_equal (sys.cfg.parent_calls_of n) expected)
      "dispatch must use state and control-only permutation, not kind/empty history";
    let fake = (trans sys "finish").tr_info in
    let changed = {n with from = [fake, [q;p], n]; depth = 1} in
    require (calls_equal (sys.cfg.parent_calls_of changed) expected)
      "unrelated history changed dispatch") [Orig; Node; Approx];
  let boundary = {n with state = Node.neutral_pos; kind = Node} in
  let yielded = sys.cfg.final_calls boundary in
  require (yielded <> []) "neutral Node with reset history must enumerate yields";
  require (List.for_all (fun e -> Hstring.equal e.evt_trans (Hstring.make "finish")) yielded)
    "neutral enumerated nonyielding entry"

let neutral_transfer sys =
  let original = List.hd sys.t_unsafe in
  let cfg = {sys.cfg with has_parents = (fun _ -> true)} in
  let rec reach depth nodes =
    require (depth > 0 && nodes <> []) "did not reach an entry";
    let nodes = List.concat_map (all_pre {sys with cfg}) nodes in
    match List.find_opt (fun n -> neutral n.state) nodes with
    | None -> reach (depth - 1) nodes
    | Some got ->
       let located = List.find (fun n -> not (neutral n.state) &&
         n.cube == got.cube && n.from == got.from) nodes in
       require (got != located && got.tag <> located.tag) "release must be fresh";
       require (got.cube == located.cube) "release lost cube/witnesses";
       require (got.from == located.from && got.depth = located.depth)
         "release added/lost diagnostic step" in
  reach 4 [original]

let mixed sys =
  let got = all_pre sys (List.hd sys.t_unsafe) in
  require (List.length got = 2) "initial/internal event did not split";
  require (List.exists (fun n -> neutral n.state && n.depth = 1) got) "missing release";
  let retained = List.find (fun n -> n.state = pos "finish" []) got in
  require (retained.depth = 1 && List.length retained.from = 1) "missing located branch";
  let parents = sys.cfg.parent_calls_of retained in
  require (parents = [event "enter" []]) "retained branch still includes neutral";
  let boundary = single (all_pre sys retained) in
  require (neutral boundary.state && boundary.depth = 2) "lost executable predecessor"

let trace sys =
  let origin = List.hd sys.t_unsafe in
  let finish = single (all_pre sys origin) in
  require (finish.state = pos "finish" [] && finish.depth = 1) "first executable depth";
  let boundary = single (all_pre sys finish) in
  require (neutral boundary.state && boundary.depth = 2)
    "crossing inserted trace step";
  require (List.length boundary.from = 2) "trace must contain exactly two executable steps"

let scheduler sys internal_first initial_match =
  let boundary = List.hd sys.t_unsafe in
  let cube = if initial_match then Cube.create [] SAtom.empty else boundary.cube in
  let internal = Node.create ~pos:(pos "finish" []) cube in
  (* A process-local ref would falsely fail if preimages run in workers. *)
  let visits = Sys.getenv "ISOQA_VISITS" in
  let record n =
    let out = open_out_gen [Open_creat; Open_append; Open_text] 0o600 visits in
    output_string out (string_of_int n.tag ^ "\n");
    close_out out;
    [] in
  let cfg = {sys.cfg with parent_calls_of = record; final_calls = record} in
  let nodes = if internal_first then [internal; boundary] else [boundary; internal] in
  match Bwd.Selected.search {sys with cfg; t_unsafe = nodes} with
  | Bwd.Unsafe _ -> failwith "internal initial-data match ran safety"
  | Bwd.Safe (visited, _) ->
    let input = open_in visits in
    let rec read acc =
      match input_line input with
      | line -> read (int_of_string line :: acc)
      | exception End_of_file -> close_in input; acc in
    let seen = read [] in
    require (List.mem internal.tag seen) "internal obligation covered before dispatch";
    require (List.mem boundary.tag seen) "internal obligation covered boundary";
    require (not boundary.deleted) "internal deleted boundary";
    require (List.exists (fun n -> n.tag = internal.tag) visited) "lost internal visited node";
    require (List.exists (fun n -> n.tag = boundary.tag) visited) "lost boundary visited obligation"

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let case = Sys.getenv "ISOQA_CASE" in
    (match case with
     | "cfg-state" -> cfg_state sys
     | "neutral-transfer" -> neutral_transfer sys
     | "mixed" -> mixed sys
     | "trace" -> trace sys
     | "boundary-first" -> scheduler sys false false
     | "internal-first" -> scheduler sys true false
     | "internal-initial" -> scheduler sys false true
     | _ -> failwith "unknown probe");
    Printf.printf "PASS %s\n%!" case
  with exn ->
    Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn);
    exit 1
