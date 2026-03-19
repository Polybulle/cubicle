exception Cycle of int list

type node = Ast.transition_info

type edge = Ast.transition_call

type path = node * ((node * edge * node) list)

let print_path fmt (src, p) =
  let open Format in
  let go_one n =
    fprintf fmt "%a(%a)"
      Hstring.print n.Ast.tr_name
      Variable.print_vars n.Ast.tr_args in
  let rec go_tail e p =
    fprintf fmt " -> %a(%a)"
        Hstring.print e.Ast.tc_name
        Variable.print_vars e.Ast.tc_args;
    match p with
    | [] -> ()
    | (t,e',_)::p' ->
      go_tail e' p' in
  match p with
  | [] -> go_one src
  |  (n,e,_)::p ->
    pp_open_hbox fmt ();
    go_one n;
    go_tail e p;
    pp_close_box fmt ()

let debug_paths ps =  if Options.debug || Options.verbose > 0 then begin
      let open Format in
      if List.length ps = 0 then
        printf "Found 0 paths through triggers in the model.@."
      else
        printf "@[<v 2>Found the following %n trigger path(s):@;%a@]@."
          (List.length ps)
          (pp_print_list ~pp_sep:pp_print_cut print_path) ps
    end

let rec path_map f = List.map (fun (n,e,n') -> (f n, e, f n'))

let path_rev ((src,p):path) = (src, List.rev_map (fun (n,e,n') -> (n',e,n)) p)

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
  val paths : path list
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

  let paths : path list =
    let rec explore src v rev_edges acc =
      G.edges_from v |> List.fold_left (fun acc e ->
          let w = G.dest_node e in
          let rev_edges' = (v, e, w) :: rev_edges in
          let acc = if G.is_output w
            then (src, List.rev rev_edges') :: acc
            else acc in
          if G.is_input w && G.is_output w
          then acc
          else explore src w rev_edges' acc
        ) acc
    in
    G.nodes
    |> Array.fold_left (fun acc s ->
        if G.is_input s then
          let acc = if G.is_output s then (s, []) :: acc else acc in
          explore s s [] acc
        else
          acc
      ) []


end
