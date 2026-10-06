(* Review addition, not part of the frozen independent suite.
   Observe real BRAB candidate selection and CFG dispatch without patching sources. *)
open Ast

let neutral n = n.state = Node.neutral_pos
let () =
  let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
  let internal = ref 0 and boundary = ref 0 and candidate_dispatch = ref 0 in
  let record calls n =
    if neutral n then incr boundary else incr internal;
    if n.kind = Approx then begin
      if not (neutral n) then failwith "internal approximation dispatched";
      incr candidate_dispatch
    end;
    calls n in
  let cfg = {sys.cfg with
    parent_calls_of = record sys.cfg.parent_calls_of;
    final_calls = record sys.cfg.final_calls} in
  match Brab.brab {sys with cfg} with
  | Bwd.Unsafe _ -> failwith "expected SAFE German model"
  | Bwd.Safe (visited, candidates) ->
    if candidates = [] || !candidate_dispatch = 0 then failwith "no actual approximation selected";
    if !boundary = 0 then failwith "missing boundary exploration";
    if not (List.for_all neutral (visited @ candidates)) then failwith "nonneutral visited/candidate";
    Printf.printf "PASS review-approx: internal=%d boundary=%d candidate_dispatch=%d candidates=%d\n%!"
      !internal !boundary !candidate_dispatch (List.length candidates)
