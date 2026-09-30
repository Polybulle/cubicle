(**************************************************************************)
(*                                                                        *)
(*                              Cubicle                                   *)
(*                                                                        *)
(*                       Copyright (C) 2011-2014                          *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                       Universite Paris-Sud 11                          *)
(*                                                                        *)
(*                                                                        *)
(*  This file is distributed under the terms of the Apache Software       *)
(*  License version 2.0                                                   *)
(*                                                                        *)
(**************************************************************************)

open Options
open Util
open Types
open Ast

module H = Hstring

(* Trie, mapping cubes to value of type 'a *)
type 'a t = 
  | Empty
  | Full of 'a
  | Node of (Atom.t * 'a t) list

let empty = Empty

(* Test emptiness of a trie *)
let is_empty = function
  | Empty -> true
  | _ -> false

(* Add a mapping cube->v to trie *)
let rec add cube v trie = match trie with
  | Empty -> List.fold_right (fun a t -> Node [a,t]) cube (Full v)
  | Full _ -> trie
  | Node l -> match cube with
      | [] -> Full v
      | atom::cube -> Node (add_to_list atom cube v l)
and add_to_list atom cube v l = match l with
  | [] -> [atom, add cube v Empty]
  | (atom',t')::n ->
      let cmp = Atom.compare atom atom' in
      if cmp = 0 then (atom, add cube v t')::n
      else if cmp > 0 then (atom',t')::(add_to_list atom cube v n)
      else (atom, add cube v Empty)::l

(* Add a mapping cube->v to trie without checking for subsomption *)
let rec add_force cube v trie = match trie with
  | Empty -> List.fold_right (fun a t -> Node [a,t]) cube (Full v)
  | Full _ -> trie
  | Node l -> match cube with
      | [] -> Full v
      | atom::cube -> Node (add_force_to_list atom cube v l)
and add_force_to_list atom cube v l = match l with
  | [] -> [atom, add_force cube v Empty]
  | (atom',t')::n ->
      let cmp = Atom.compare atom atom' in
      if cmp > 0 then (atom',t')::(add_force_to_list atom cube v n)
      else (atom, add_force cube v Empty)::l

(* (\* Add a mapping cube->v to trie *\) *)
(* let rec add_array cube v trie = match trie with *)
(*   | Empty -> Array.fold_right (fun a t -> Node [a,t]) cube (Full v) *)
(*   | Full _ -> trie *)
(*   | Node l ->  *)
(*       if Array.length cube = 0 then Full v *)
(*       else Node (add_array_to_list  *)
(*                    cube.(0) (Array.sub cube 1 (Array.length cube - 1)) *)
(*                    v l) *)
(* and add_array_to_list atom cube v l = match l with *)
(*   | [] -> [atom, add_array cube v Empty] *)
(*   | (atom',t')::n -> *)
(*       let cmp = Atom.compare atom atom' in *)
(*       if cmp = 0 then (atom, add_array cube v t')::n *)
(*       else if cmp > 0 then (atom',t')::(add_array_to_list atom cube v n) *)
(*       else (atom, add_array cube v Empty)::l *)

(* (\* Add a mapping cube->v to trie without checking for subsomption *\) *)
(* let rec add_array_force cube v trie = match trie with *)
(*   | Empty -> Array.fold_right (fun a t -> Node [a,t]) cube (Full v) *)
(*   | Full _ -> trie *)
(*   | Node l ->  *)
(*       if Array.length cube = 0 then Full v *)
(*       else Node (add_array_force_to_list  *)
(*                    cube.(0) (Array.sub cube 1 (Array.length cube - 1)) *)
(*                    v l) *)
(* and add_array_force_to_list atom cube v l = match l with *)
(*   | [] -> [atom, add_array_force cube v Empty] *)
(*   | (atom',t')::n -> *)
(*       let cmp = Atom.compare atom atom' in *)
(*       if cmp > 0 then (atom',t')::(add_array_force_to_list atom cube v n) *)
(*       else (atom, add_array_force cube v Empty)::l *)


let add_array cube v trie = add (Array.to_list cube) v trie

let add_array_force cube v trie = add_force (Array.to_list cube) v trie

(* Is cube subsumed by some cube in the trie? *)
let rec find cube trie = match trie with
  | Empty -> None
  | Full v -> Some v
  | Node l -> match cube with
      | [] -> None
      | atom::cube -> 
          find_list atom cube l
and find_list atom cube l = match l with
  | [] -> None
  | (atom',t')::n ->
      (* let cmp = Atom.compare atom atom' in *)
      let cmp = - (Atom.trivial_is_implied atom' atom) in
      if cmp = 0 then match find cube t' with
        | Some _ as r -> r
        | None -> match cube with
            | [] -> None
            | atom::cube -> find_list atom cube l
      else if cmp > 0 then find_list atom cube n
      else match cube with
          | [] -> None
          | atom::cube -> find_list atom cube l


let mem cube trie = match find cube trie with
  | None -> None
  | Some n -> Some [n.tag]

let rec mem_poly cube trie = match trie with 
  | Empty -> false
  | Full _ -> true
  | Node l -> match cube with
      | [] -> false
      | atom::cube -> 
          mem_poly_list atom cube l
and mem_poly_list atom cube l = match l with
  | [] -> false
  | (atom',t')::n ->
      (* let cmp = Atom.compare atom atom' in *)
      let cmp = - (Atom.trivial_is_implied atom' atom) in
      if cmp = 0 then match mem_poly cube t' with
        | true -> true
        | false -> match cube with
            | [] -> false
            | atom::cube -> mem_poly_list atom cube l
      else if cmp > 0 then mem_poly_list atom cube n
      else match cube with
          | [] -> false
          | atom::cube -> mem_poly_list atom cube l

(* (\* Is cube subsumed by some cube in the trie? *\) *)
(* let rec mem_array cube trie =  *)
(*   match trie with  *)
(*   | Empty -> None *)
(*   | Full { tag = id } -> Some [id] *)
(*   | Node l -> *)
(*       if Array.length cube = 0 then None *)
(*       else mem_array_list *)
(*         cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l *)
(* and mem_array_list atom cube l = match l with *)
(*   | [] -> None *)
(*   | (atom',t')::n -> *)
(*       (\* let cmp = Atom.compare atom atom' in *\) *)
(*       let cmp = - (Atom.trivial_is_implied atom' atom) in *)
(*       if cmp = 0 then  *)
(*         match mem_array cube t' with *)
(*           | None ->  *)
(*               if Array.length cube = 0 then None *)
(*               else mem_array_list *)
(*                 cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l *)
(*           | Some _ as r -> r *)
(*       else if cmp > 0 then mem_array_list atom cube n *)
(*       else if Array.length cube = 0 then None *)
(*       else mem_array_list *)
(*         cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l *)


let mem_array a trie = mem (Array.to_list a) trie

(* (\* Is cube subsumed by some cube in the trie? *\) *)
(* let rec mem_array_poly cube trie =  *)
(*   match trie with  *)
(*   | Empty -> false *)
(*   | Full _ -> true *)
(*   | Node l -> *)
(*       if Array.length cube = 0 then false *)
(*       else mem_array_poly_list *)
(*         cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l *)
(* and mem_array_poly_list atom cube l = match l with *)
(*   | [] -> false *)
(*   | (atom',t')::n -> *)
(*       (\* let cmp = Atom.compare atom atom' in *\) *)
(*       let cmp = - (Atom.trivial_is_implied atom' atom) in *)
(*       if cmp = 0 then  *)
(*         mem_array_poly cube t' || *)
(*           (Array.length cube <> 0 && *)
(*              mem_array_poly_list *)
(*                cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l *)
(*           ) *)
(*       else if cmp > 0 then mem_array_poly_list atom cube n *)
(*       else (Array.length cube <> 0 && *)
(*               mem_array_poly_list *)
(*                 cube.(0) (Array.sub cube 1 (Array.length cube - 1)) l) *)

let mem_array_poly a trie = mem_poly (Array.to_list a) trie

let mem c t =
  TimerSubset.start ();
  let res = mem c t in
  TimerSubset.pause ();
  res
   
(* Apply f to all values mapped to in the trie. *)
let rec iter f trie = match trie with
  | Empty -> ()
  | Full v -> f v
  | Node l -> List.iter (fun (_,t) -> iter f t) l

let mem_array c t =
  (* Format.eprintf "memarray %a in@." Pretty.print_array c; *)
  (* iter (fun s -> Format.eprintf ">>> %a\n@." Pretty.print_array s.t_arru) t; *)
  TimerSubset.start ();
  let res = mem_array c t in
  TimerSubset.pause ();
  res

(* Fold f to all values mapped to in the trie. *)
let rec fold f acc trie = match trie with
  | Empty -> acc
  | Full v -> f acc v
  | Node l -> List.fold_left (fun acc (_,t) -> fold f acc t) acc l

(* Apply f to all values whose keys (cubes) are subsumed by the given cube. *)
let rec iter_subsumed f cube trie = match cube, trie with
  | [], _ -> iter f trie
  | _, Empty 
  | _, Full _ -> () 
  | _, Node l -> iter_subsumed_list f cube l
and iter_subsumed_list f cube l = match l with
  | [] -> ()
  | (atom',t')::n ->
      let atom = List.hd cube in
      let cmp = Atom.compare atom atom' in
      if cmp=0 then 
        iter_subsumed f (List.tl cube) t'
      else if cmp>0 then begin
        iter_subsumed f cube t';
        iter_subsumed_list f cube n 
      end

(* Delete all values which satisfy the predicate p *)
let rec delete p trie = match trie with 
  | Empty -> Empty
  | Full v -> if p v then Empty else trie
  | Node l -> 
      let l' = delete_list p l in
      if l == l' then trie else Node l'
and delete_list p l = match l with
  | [] -> []
  | (atom, t):: n ->
      let t' = delete p t in
      let n' = delete_list p n in
      if t'==t && n'==n then l else (atom,t')::n'


(* List of all values mapped by the trie *)
let rec all_vals = function
  | Empty -> []
  | Full v -> [v]
  | Node l -> 
      List.flatten (List.fold_left (fun acc (_,t) -> (all_vals t)::acc) [] l)

(* All values whose keys (cubes) are not inconsistent with the given cube. *)
let rec consistent cube trie = match cube, trie with
  | [], _ -> all_vals trie
  | _, Empty -> []
  | _, Full v -> [v]
  | (atom::cube), Node l -> List.flatten (List.map (consistent_list atom cube) l)
and consistent_list atom cube ((atom', t') as n) = match (atom, atom') with
  | Atom.Comp (Elem (v1, Glob), Eq, Elem (x1, Constr)),
    Atom.Comp (Elem (v2, Glob), Eq, Elem (x2, Constr))
  | Atom.Comp (Elem (v1, Glob), Eq, Elem (x1, Var)),
    Atom.Comp (Elem (v2, Glob), Eq, Elem (x2, Var)) 
      when H.equal v1 v2 && not (H.equal x1 x2) ->
      []
  | Atom.Comp (Elem (v1, Glob), Eq, Elem (x1, Constr)),
    Atom.Comp (Elem (v2, Glob), (Neq|Lt), Elem (x2, Constr))
      when H.equal v1 v2 && H.equal x1 x2 ->
      []
  | Atom.Comp (Access (a1,li1), Eq, (Elem (_,(Constr|Glob)) | Arith _ as x1)),
    Atom.Comp (Access (a2,li2), Eq, (Elem (_,(Constr|Glob)) | Arith _ as x2))
      when H.equal a1 a2 && H.list_equal li1 li2 && Term.compare x1 x2 <> 0 ->
      []
  | Atom.Comp (Access (a1,li1), Eq,
               (Elem (_, (Constr|Glob)) | Arith _ as x1)),
    Atom.Comp (Access (a2,li2), (Neq | Lt), 
               (Elem (_, (Constr|Glob)) | Arith _ as x2))
      when H.equal a1 a2 && H.list_equal li1 li2 && Term.compare x1 x2 = 0 ->
      []
  | _, _ ->
      let cmp = Atom.compare atom atom' in
      if cmp = 0 then consistent cube t'
      else if cmp < 0 then match cube with
        | [] -> all_vals t'
        | atom::cube -> consistent_list atom cube n
      else consistent (atom::cube) t'

let delete_subsumed ?(cpt=ref 0) ?normalized p nodes =
  let normalized = match normalized with
    | Some n -> n | None -> Node.normalize ~with_state:false p in
  let vars, ap = Node.variables normalized, Node.array normalized in
  let substs = Variable.all_permutations vars vars in
  List.iter (fun ss ->
    let u = ArrayAtom.apply_subst ss ap in
    iter_subsumed (fun n ->
      if Node.has_deleted_ancestor n || not (Node.ancestor_of n p) then begin
        n.deleted <- true;
        if dot then Dot.delete_node_by n p;
        incr cpt;
      end
    ) (Array.to_list u) nodes;
  ) substs;
  (* iter (fun n -> if Node.has_deleted_ancestor n then *)
  (*       n.deleted <- true; *)
  (*       ) nodes; *)
  delete (fun n -> n.deleted || Node.has_deleted_ancestor n) nodes


let add_node ?normalized p trie =
  let normalized = match normalized with
    | Some n -> n | None -> Node.normalize ~with_state:false p in
  add_array (Node.array normalized) p trie

module type S = sig
  type t
  val empty : t
  val add_node : ?normalized:Node.t -> Node.t -> t -> t
  val mem : Node.t -> t -> int list option
  val fold : ('a -> Node.t -> 'a) -> 'a -> t -> 'a
  val fold_at : Node.t -> ('a -> Node.t -> 'a) -> 'a -> t -> 'a
  val all_vals : t -> Node.t list
  val delete : (Node.t -> bool) -> t -> t
  val delete_subsumed : ?cpt:int ref -> ?normalized:Node.t -> Node.t -> t -> t
end

module Ordinary = struct
  type nonrec t = Node.t t
  let empty = empty
  let add_node = add_node
  let mem n nodes = mem_array (Node.array n) nodes
  let fold = fold
  let fold_at _ f acc nodes = fold f acc nodes
  let all_vals = all_vals
  let delete = delete
  let delete_subsumed = delete_subsumed
end

module Located = struct

  module M = Map.Make(Hstring)
  type bucket = (Node.t * Node.t) t
  type t = bucket M.t

  let empty = M.empty
  let empty_bucket = Empty

  let key n =
    let Before e = n.state in
    e.evt_trans

  let is_canonical_pos n =
    let Before e = n.state in
    e.evt_args = Variable.give_procs (List.length e.evt_args)

  let bucket k nodes = try M.find k nodes with Not_found -> empty_bucket

  let add_node ?normalized n nodes =
    let normalized = match normalized with
      | Some n -> n | None -> Node.normalize n in
    let k = key normalized in
    let b = bucket k nodes in
    M.add k (add_array (Node.array normalized) (n, normalized) b) nodes

  let mem n nodes =
    if not (is_canonical_pos n) then None else begin
      TimerSubset.start ();
      let res = match find (Array.to_list (Node.array n)) (bucket (key n) nodes) with
        | None -> None
        | Some (n, _) -> Some [n.tag] in
      TimerSubset.pause ();
      res
    end

  let fold_at n f acc nodes =
    Node.validate_posititon n;
    fold (fun acc (_, norm) -> f acc norm) acc (bucket (key n) nodes)

  let fold f acc nodes = M.fold (fun _ b acc ->
    fold (fun acc (n, _) -> f acc n) acc b) nodes acc

  let all_vals nodes = fold (fun acc n -> n :: acc) [] nodes

  let delete pred nodes =
    M.map (delete (fun (n, _) -> pred n)) nodes

  let delete_subsumed ?(cpt=ref 0) ?normalized p nodes =
    let normalized = match normalized with
      | Some n -> n | None -> Node.normalize p in
    let Before e = normalized.state in
    let vars = List.filter (fun v -> not (Hstring.list_mem v e.evt_args))
        (Node.variables normalized) in
    let substs = Variable.all_permutations vars vars in
    ListLabels.iter substs ~f:(fun sigma ->
      let permuted = Node.subst sigma normalized in
      let mark n =
        if not n.deleted &&
           (Node.has_deleted_ancestor n || not (Node.ancestor_of n p)) then begin
          n.deleted <- true;
          if dot then Dot.delete_node_by n p;
          incr cpt
        end in
      let b = bucket (key permuted) nodes in
      iter_subsumed (fun (n, _) -> mark n) (Array.to_list (Node.array permuted)) b);
    (* Descendant cleanup follows dependencies, not control locations. *)
    delete (fun n -> n.deleted || Node.has_deleted_ancestor n) nodes

end

module Selected = (val (if tx_bwd then (module Located : S)
                       else (module Ordinary : S)))
