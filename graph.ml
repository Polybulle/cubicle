exception Cycle of int list

type node = Ast.transition_info

type edge = Ast.transition_call

(** Type of paths through triggered transitions. *)
(**  Parameter [tr] is the transition info type *)
type 'tr path =
  | Tcp_one of 'tr
  | Tcp_step of 'tr * Ast.transition_call * ('tr path)

let print_path tr_info fmt p =
  let open Format in
  let go_one n =
    let n = tr_info n in
    fprintf fmt "%a(%a)"
      Hstring.print n.Ast.tr_name
      Variable.print_vars n.Ast.tr_args in
  let rec go_tail e p =
    fprintf fmt " -> %a(%a)"
        Hstring.print e.Ast.tc_name
        Variable.print_vars e.Ast.tc_args;
    match p with
    | Tcp_one n -> ()
    | Tcp_step (t,e',p') ->
      go_tail e' p' in
  match p with
  | Tcp_one t -> go_one t
  | Tcp_step (n,e,p) ->
    pp_open_hbox fmt ();
    go_one n;
    go_tail e p;
    pp_close_box fmt ()

let rec path_map f = function
  | Tcp_one t -> Tcp_one (f t)
  | Tcp_step (t,e,p) -> Tcp_step (f t, e, path_map f p)

let path_rev p =
  let rec aux acc e = function
    | Tcp_one t -> Tcp_step (t,e,acc)
    | Tcp_step (t',e,p) -> aux (Tcp_step (t',e,acc)) e p in
  match p with
  | Tcp_one _ -> p
  | Tcp_step (t,e,p) -> aux (Tcp_one t) e p

module type DAG = sig
  val nodes : node array
  val is_input : node -> bool
  val is_output : node -> bool
  val edges_from : node -> edge list
  val dest_node : edge -> node
end

let array_findi a x =
  let exception Found of int in
  try
    Array.iteri (fun i y -> if x == y then raise (Found i)) a;
    raise Not_found
  with Found i -> i


module type Algos = sig
  val is_acyclic : bool
  val paths : node path list
end


module Make (G : DAG) = struct

  let idx_of_node n = array_findi G.nodes n

  let neighboors i =
    List.map (fun e -> idx_of_node (G.dest_node e)) (G.edges_from G.nodes.(i))


  (* cycle detection  *)

  type mark = NotSeen | Current | Done

  let is_acyclic =

    let marks = Array.make (Array.length G.nodes) NotSeen in

    let rec visit i =
      match marks.(i) with
      | Done -> ()
      | Current -> raise (Cycle [])
      | NotSeen -> begin
          marks.(i) <- Current;
          List.iter visit (neighboors i);
          marks.(i) <- Done;
        end in

    begin
      try for n = 0 to Array.length G.nodes -1 do
          visit n
        done
      with Cycle _ ->
        let involved = ref [] in
        for i = 0 to Array.length G.nodes -1 do
          if marks.(i) = Current then
            involved := i :: !involved
        done;
        raise (Cycle !involved)
    end;

    true


  (* compute all paths *)

  module M = Map.Make (struct
      type t = int
      let compare = compare
    end)

  let paths =

    let paths : node path list M.t ref = ref M.empty in

    let rec visit i =
      if M.mem i !paths then
        ()
      else
        let n = G.nodes.(i) in
        List.iter visit (neighboors i);
        let ps = List.concat_map
            (fun e ->
               let j = idx_of_node (G.dest_node e) in
               let ps = Option.value (M.find_opt j !paths) ~default:[] in
               List.map (fun p -> Tcp_step (n,e,p)) ps)
            (G.edges_from n) in
        let ps = if G.is_output n then (Tcp_one n)::ps else ps in
        let ps = try ps @ M.find i !paths with Not_found -> ps in
        paths := M.add i ps !paths
    in

    for i = 0 to Array.length G.nodes -1 do
      if G.is_input G.nodes.(i) then visit i
    done;

   M.fold
       (fun i ps acc ->
         if G.is_input G.nodes.(i) then
           List.append ps acc
         else
           acc)
       !paths
       []

  let _ = if Options.debug || Options.verbose > 0 then begin
      let open Format in
      printf "@[<v>Found the following %n trigger path(s):@.%a@.@.@]"
        (List.length paths)
        (pp_print_list ~pp_sep:pp_print_newline (print_path (fun n->n))) paths
    end

end
