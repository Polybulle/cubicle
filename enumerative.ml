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
open Format
open Ast
open Types
open Util

(** Hash table keyed by terms *)
module HT = Hashtbl.Make (Term)

module HI = Hashtbl.Make (struct 
  type t = int 
  let equal = (=) 
  let hash x = x
end)

module HLI = Hashtbl.Make (struct 
  type t = int list 
  let equal = (=) 
  let hash = Hashtbl.hash
end)

module SLI = Set.Make (struct
    type t = int list
    let compare = Stdlib.compare
  end)

type state = int array

let hash_state st = Hashtbl.hash_param 100 500 st

module HST = Hashtbl.Make 
  (struct 
    type t = state
    let equal = (=)
    let hash = hash_state
   end)


(* This is a queue with a hash table on the side to avoid storing useless
   states, the overhead of the hashtable is negligible and allows to reduce the
   memory occupied by the queue (which is generally a lot larger than the state
   table for BFS) *)
module HQueue = struct

  type t = (int * state) Queue.t * unit HST.t

  let create size = Queue.create (), HST.create size

  let add ?(cpt_q=ref 0) x (q, h) =
    let s = snd x in
    if not (HST.mem h s) then begin
      incr cpt_q;
      HST.add h s ();
      Queue.add x q;
    end

  let is_empty (q, _) = Queue.is_empty q

  let take (q, h) =
    let x = Queue.take q in
    HST.remove h (snd x);
    x
  
end




type st_req = int * op_comp * int

type st_action =
  | St_ignore
  | St_assign of int * int
  | St_arith of int * int * int
  | St_ite of st_req list * st_action * st_action


(* Raised when a transition's guards are not satisfied by the current state *)
exception Not_applicable

(* A compiled transition, ready for execution on concrete states.
   [st_f] may raise [Not_applicable] *)
type state_transistion = {
  st_name : Hstring.t;
  st_reqs : st_req list;
  st_udnfs : st_req list list list;
  st_actions : st_action list;
  st_f : state -> state list;
}


type state_transaction = state_transistion list

(** Environment for enumerative exploration.
    This record contains all the mappings and state needed to execute
    transitions on concrete states. Symbolic terms
    are encoded as integers, and states are int arrays where:
    - Indices 0..max_id_vars are state variables (globals, arrays)
    - Indices first_proc..extra_proc-1 are process identifiers
    - Higher indices are constructors and constants
*)
type env = {
  model_cardinal : int;     (** Number of processes in the finite model *)
  nb_vars : int;            (** Number of state variables (array size) *)
  max_id_vars : int;        (** Highest index that is a state variable *)
  first_proc : int;         (** Index of first process ID (#1) *)
  extra_proc : int;         (** Index of extra process (for disequalities) *)
  all_procs : Hstring.t list;  (** All process names including extra *)
  proc_ids : int list;      (** Encoded IDs of regular processes *)
  id_terms : int HT.t;      (** Map from terms to their integer encoding *)
  id_true : int;            (** Encoded ID for True constant *)
  id_false : int;           (** Encoded ID for False constant *)
  st_trs : state_transistion list;  (** Compiled transitions *)
  st_tracts : state_transaction list;
  low_int_abstr : int;      (** Lower bound of abstracted int range *)
  up_int_abstr : int;       (** Upper bound of abstracted int range *)
  pinf_int_abstr : int;     (** Encoding for +infinity *)
  minf_int_abstr : int;     (** Encoding for -infinity (always -3) *)
  proc_substates : int list HLI.t;     (** For in-place substitutions on arrays *)
  reverse_proc_substates : int list HI.t;  (** Reverse mapping for substitutions *)
  table_size : int;         (** Estimated size for hash tables *)
  mutable explicit_states : unit HST.t;  (** Visited states (for dedup during BFS) *)
  mutable states : state list;  (** All explored states (for candidate checking) *)
}

let empty_env = {
  model_cardinal = 0;
  max_id_vars = 0;
  nb_vars = 0;
  first_proc = 0;
  extra_proc = 0;
  all_procs = [];
  proc_ids = [];
  id_terms = HT.create 0;
  id_true = 0;
  id_false = 0;
  st_trs = [];
  st_tracts = [];
  low_int_abstr = 0;
  up_int_abstr = 0;
  pinf_int_abstr = 0;
  minf_int_abstr = 0;
  proc_substates = HLI.create 0;
  reverse_proc_substates = HI.create 0;

  table_size = 0;
  explicit_states = HST.create 0;
  states = [];
}



(* inefficient but only used for debug *)
let id_to_term env id =
  let exception Found of term in
  try
    HT.iter (fun t i -> if id = i then raise (Found t)) env.id_terms;
    raise Not_found
  with Found t -> t

(* inefficient but only used for debug *)
let state_to_cube env st =
  let i = ref 0 in
  Array.fold_left (fun sa sti ->
    let sa = 
      if sti <> -1 then
	let t1 = id_to_term env !i in
	let t2 =
          if sti = env.minf_int_abstr then Elem (Hstring.make "-oo", Constr)
          else if sti = env.pinf_int_abstr then Elem (Hstring.make "+oo", Constr) 
          else id_to_term env sti in
	SAtom.add (Atom.Comp (t1, Eq, t2)) sa
      else sa
    in
    incr i; sa)
    SAtom.empty st

let print_state env fmt st = SAtom.print fmt (state_to_cube env st)


let swap a i j =
  if i <> j then
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
    
(* Applying substitutions in place is tricky because some idexes of the array
   encode terms like A[#1,#2]. We proceed by swapping here, and remember the
   shifting introduced thanks to the mapping rho. *)
let apply_subst_in_place env st sigma =
  if not (HI.length sigma = 0) then begin

    let proc_subs = ref SLI.empty in
    let rho = HLI.create env.nb_vars in

    (* First apply substitutions in the values of the state variables *)
    for i = 0 to env.nb_vars - 1 do
      (try st.(i) <- HI.find sigma st.(i) 
       with Not_found -> ());

      try
        (* collect process domains (like (#1, #2) ) *)
        let proc_domain = HI.find env.reverse_proc_substates i in
        proc_subs := SLI.add proc_domain !proc_subs;
      with Not_found -> ()

    done;

    SLI.iter (fun proc_domain ->
        try
          (* sigma(proc_domain) *)
          let sigma_proc_domain = List.fold_left (fun acc j ->
              try HI.find sigma j :: acc
              with Not_found -> acc
            ) [] proc_domain |> List.rev in
          (* rho(proc_domain) *)
          let rho_proc_domain =
            try HLI.find rho proc_domain with Not_found -> sigma_proc_domain in
          (* encoding in terms of indexes *)
          let sigma_proc_sub = HLI.find env.proc_substates sigma_proc_domain in
          let rho_proc_sub = HLI.find env.proc_substates rho_proc_domain in
         
          (* Perform actual swaps on the encoded versions *)
          List.iter2 (fun i j ->
              (* eprintf "   exchanging %a <---> %a@."
                 Term.print (id_to_term env i) Term.print (id_to_term env j); *)
              swap st i j) sigma_proc_sub rho_proc_sub;

          (* rho += sigma(proc_domain) |--> rho(proc_domain) *)
          HLI.replace rho sigma_proc_domain rho_proc_domain;
        with Not_found ->()
      ) !proc_subs

  end

let is_proc env v = env.first_proc <= v && v < env.extra_proc




let find_subst_for_norm env st =
  let met = ref [] in
  let remaining = ref env.proc_ids in
  let sigma = HI.create env.model_cardinal in
  for i = 0 to Array.length st - 1 do
    let v = st.(i) in
    match !remaining with
    | r :: tail ->
      if is_proc env v && v <> env.extra_proc && (* r <> env.extra_proc && *)
         not (List.mem v !met) then begin
        met := v :: !met;
        remaining := tail;
        if v <> r then HI.add sigma v r;
      end
    | _ -> ()
  done;
  let not_met = List.filter (fun v -> not (List.mem v !met)) env.proc_ids in
  List.iter2 (fun v r -> if v <> r then HI.add sigma v r) not_met !remaining;
  sigma


let normalize_state env st =
  let sigma = find_subst_for_norm env st in
  apply_subst_in_place env st sigma (* ; *)


let global_envs = ref []



let make_range (low, up) =
  let l = ref [] in
  for i = up downto low do
    l := i :: !l
  done;
  !l

let abstr_range = make_range num_range

let abstr_add env x y =
  let r =
    if x = env.minf_int_abstr then
      if y <> env.pinf_int_abstr then x
      else -1 (* raise Not_found *)
    else if x = env.pinf_int_abstr then
      if y <> env.minf_int_abstr then x
      else -1 (* raise Not_found *)
    else
      if y = env.pinf_int_abstr || y = env.minf_int_abstr then y
      else x + y in
  if r < env.low_int_abstr then env.minf_int_abstr
  else if r > env.up_int_abstr then env.pinf_int_abstr
  else r

let abstr_add env x y =
  let r = abstr_add env x y in
  if r = env.minf_int_abstr || r = env.pinf_int_abstr then raise Not_applicable;
  r


let is_variable env id = id <= env.max_id_vars

let all_constr_terms () =
  List.rev_map (fun x -> Elem (x, Constr)) (Smt.Type.all_constructors ())

let terms_of_procs = List.map (fun x -> Elem (x, Var))


let rec power_p i p = if p <= 0 then 1 else i * power_p i (p-1)

let table_size nb_procs nb_vars =
  let r = min 2_000_009
  (max 100 ((power_p (nb_procs * nb_vars) nb_procs) * (nb_procs ))) in
  if not quiet then eprintf "table size : %d@." r;
  r

let add_pos_to_proc_substate ht
    proc_substates reverse_proc_substates =
  Term.Set.iter (function
      | Access (_, ps) as t ->
        let i = HT.find ht t in
        let ids_ps = List.map (fun hp -> HT.find ht (Elem (hp, Var))) ps in
        let sub_ps = try HLI.find proc_substates ids_ps with Not_found -> [] in
        HLI.replace proc_substates ids_ps (i :: sub_ps);
        HI.add reverse_proc_substates i ids_ps
      | _ -> ()
    )


let init_tables ?(alloc=true) procs s =
  let var_terms = Forward.all_var_terms procs s in
  let proc_terms = terms_of_procs procs in (* constantes *)
  let constr_terms = all_constr_terms () in (* constantes *)
  let nb_vars = Term.Set.cardinal var_terms in
  let nb_procs = List.length proc_terms in
  let nb_consts = nb_procs + List.length constr_terms in
  let ht = HT.create (nb_vars + nb_consts) in
  let i = ref 0 in
  Term.Set.iter (fun t -> HT.add ht t !i; incr i) var_terms;
  let max_id_vars = !i - 1 in
  let proc_ids = ref [] in
  let first_proc = !i in
  List.iter (fun t -> HT.add ht t !i; proc_ids := !i :: !proc_ids; incr i)
    proc_terms;
  (* add an extra process in case we need it : change this to statically compute
     how many extra processes are needed *)
  let ep = List.nth Variable.procs nb_procs in
  let all_procs = procs @ [ep] in
  HT.add ht (Elem (ep, Var)) !i;
  let extra_proc = !i in
  incr i;

  List.iter (fun t -> HT.add ht t !i; incr i) constr_terms;
  let proc_ids = List.rev !proc_ids in
  if debug then
    HT.iter (fun t i -> eprintf "%a -> %d@." Term.print t i ) ht;
  let id_true =
    try HT.find ht (Elem (Term.htrue, Constr)) with Not_found -> -2 in
  let id_false =
    try HT.find ht (Elem (Term.hfalse, Constr)) with Not_found -> -2 in
   
  let a_low = !i in
  List.iter (fun c ->
    HT.add ht (Const (MConst.add (ConstInt (Num.Int c)) 1 MConst.empty)) !i;
    HT.add ht (Const (MConst.add (ConstReal (Num.Int c)) 1 MConst.empty)) !i;
    incr i) abstr_range;
  let a_up = !i - 1 in

  (* This is some bookeeping to allow in place substitutions *)
  let proc_substates = HLI.create nb_procs in
  let reverse_proc_substates = HI.create nb_procs in
  add_pos_to_proc_substate ht proc_substates reverse_proc_substates var_terms; 

  let tsize = table_size nb_procs nb_vars in
  
  { model_cardinal = nb_procs;
    nb_vars = nb_vars;
    max_id_vars = max_id_vars;
    first_proc = first_proc;
    extra_proc = extra_proc;
    all_procs = all_procs;
    proc_ids = proc_ids;
    id_terms = ht;
    id_true = id_true;
    id_false = id_false;
    st_trs = [];
    st_tracts = [];

    low_int_abstr = a_low;
    up_int_abstr = a_up;
    pinf_int_abstr = a_up + 1;
    minf_int_abstr = -3;

    proc_substates = proc_substates;
    reverse_proc_substates = reverse_proc_substates;

    table_size = tsize;
    explicit_states = HST.create (if alloc then tsize else 0);
    states = [];
  }



let abs_inf = 
  SAtom.filter (function
    | Atom.Comp ((Elem (x, Glob) | Access (x,_)), _, _) ->
	if abstr_num then not (Smt.Symbol.has_abstract_type x)
        else not (Smt.Symbol.has_infinite_type x)
    | _ -> true)


let make_init_cdnf args lsa lvars =
  match args, lvars with
    | [], _ ->   
	[lsa]
    | _, [] ->
        [List.map 
            (SAtom.filter (fun a -> 
              not (List.exists (fun z -> Atom.has_var z a) args)))
            lsa]
    | _ ->
        let lsigs = Variable.all_instantiations args lvars in
        List.fold_left (fun conj sigma ->
          let dnf = List.fold_left (fun dnf sa ->
            let sa = abs_inf sa in
            let sa = SAtom.subst sigma sa in
            try (Cube.simplify_atoms sa) :: dnf
            with Exit -> dnf
          ) [] lsa in
          dnf :: conj
        ) [] lsigs

let rec cdnf_to_dnf_rec acc = function
  | [] -> acc
  | [] :: r ->
      cdnf_to_dnf_rec acc r
  | dnf :: r ->
      let acc = 
        List.flatten (List.rev_map (fun sac -> 
          List.rev_map (SAtom.union sac) dnf) acc) in
      cdnf_to_dnf_rec acc r

let cdnf_to_dnf = function
  | [] -> [SAtom.singleton Atom.False]
  | l -> cdnf_to_dnf_rec [SAtom.singleton Atom.True] l

let mkinits procs ({t_init = ia, l_init}) =
  let lsa = cdnf_to_dnf (make_init_cdnf ia l_init procs) in
  (* add_sorts procs *) lsa


let int_of_const = function
  | ConstInt n -> Num.int_of_num n
  | ConstReal n -> Num.int_of_num (Num.integer_num n)
  | ConstName _ -> 1

let int_of_consts cs =
  MConst.fold (fun c i acc -> i * (int_of_const c) + acc) cs 0

let write_atom_to_states env sts = function
  | Atom.Comp (t1, (Le | Lt as op), (Const _ as t2)) when abstr_num ->
      let v2 = HT.find env.id_terms t2 in
      let i1 = HT.find env.id_terms t1 in
      let l = ref [] in
      for i2 = env.low_int_abstr to (if op = Lt then v2 - 1 else v2) do
        List.iter (fun st ->
          let st = Array.copy st in
          st.(i1) <- i2;
          l := st :: !l
        ) sts
      done;
      !l
  | Atom.Comp ((Const _ as t1), (Le | Lt as op), t2) when abstr_num  ->
      let v1 = HT.find env.id_terms t1 in
      let i2 = HT.find env.id_terms t2 in
      let l = ref [] in
      for i1 = (if op = Lt then v1 + 1 else v1) to env.up_int_abstr do
        List.iter (fun st ->
          let st = Array.copy st in
          st.(i2) <- i1;
          l := st :: !l
        ) sts
      done;
      !l
  | Atom.Comp (t1, Eq, t2) ->
      List.iter (fun st -> 
        st.(HT.find env.id_terms t1) <- HT.find env.id_terms t2) sts;
      sts
  | Atom.Comp (t1, Neq, Elem(_, Var)) ->
      (* Assume an extra process if a disequality is mentioned on
         type proc in init formula : change this to something more robust *)
      List.iter (fun st -> st.(HT.find env.id_terms t1) <- env.extra_proc) sts;
      sts
  | _ -> sts
  
let write_cube_to_states env st sa =
  SAtom.fold (fun a sts -> write_atom_to_states env sts a) sa [st]

let init_to_states env procs s =
  let nb = env.nb_vars in
  let l_inits = mkinits procs s in
  let sts =
    List.fold_left (fun acc init -> 
      let st_init = Array.make nb (-1) in
      let sts = write_cube_to_states env st_init init in
      List.rev_append sts acc
    ) [] l_inits in
  List.map (fun st -> 0, st) sts
  

let atom_to_st_req env = function
  | Atom.Comp (t1, op, t2) -> 
    HT.find env.id_terms t1, op, HT.find env.id_terms t2
  | Atom.True -> raise Not_found
  | Atom.False -> env.id_true, Eq, env.id_false
  | _ -> assert false

let satom_to_st_req env sa =
  SAtom.fold (fun a acc -> 
    try (atom_to_st_req env a) :: acc
    with Not_found -> acc) sa []

type trivial_cond = Trivial of bool | Not_trivial

let trivial_cond env (i, op, v) =
  if env.first_proc <= i && i <= env.extra_proc && 
     env.first_proc <= v && v <= env.extra_proc then 
    match op with
      | Eq -> Trivial (i = v)
      | Neq -> Trivial (i <> v)
      | Le -> Trivial (i <= v)
      | Lt -> Trivial (i <> v)
  else Not_trivial

let trivial_conds env l =
  let some_non_trivial = ref false in
  if l = [] then Trivial false
  else
    try
      List.iter (fun c -> match trivial_cond env c with
	| Trivial true -> ()
	| Trivial false -> raise Exit
	| Not_trivial -> some_non_trivial := true
      ) l;
      if !some_non_trivial then Not_trivial else Trivial true
    with 
      | Exit -> Trivial false

let swts_to_stites env at sigma swts =
  let rec sd acc = function
    | [] -> assert false
    | [d] -> acc, d
    | s::r -> sd (s::acc) r in
  let swts, (_, t) = sd [] swts in
  let t = Term.subst sigma t in
  let default =
    try match t with
        | Arith (t', cs) ->
           St_arith (HT.find env.id_terms at,
                     HT.find env.id_terms t', int_of_consts cs)            
        | _ ->
           St_assign (HT.find env.id_terms at, HT.find env.id_terms t)
    with Not_found -> St_ignore
  in
  List.fold_left (fun ites (sa, t) ->
    let sa = SAtom.subst sigma sa in
    let t = Term.subst sigma t in
    let sta = 
      try match t with
          | Arith (t', cs) ->
             St_arith (HT.find env.id_terms at, 
                       HT.find env.id_terms t', int_of_consts cs)
          | _ ->
             St_assign (HT.find env.id_terms at, HT.find env.id_terms t)
      with Not_found -> St_ignore
    in
    let conds = satom_to_st_req env sa in
    match trivial_conds env conds with
    | Trivial true -> sta
    | Trivial false -> ites
    | Not_trivial -> St_ite (satom_to_st_req env sa, sta, ites)
  ) default swts


let assigns_to_actions env sigma acc tr_assigns =
  List.fold_left 
    (fun acc (h, gu) ->
      let nt = Elem (h, Glob) in
      match gu with
      | UTerm t ->
         let t = Term.subst sigma t in
         begin
           try 
             let a = match t with
               | Arith (t', cs) ->
                  St_arith (HT.find env.id_terms nt,
                            HT.find env.id_terms t', int_of_consts cs)
               | _ ->
                  St_assign (HT.find env.id_terms nt, HT.find env.id_terms t)
             in a :: acc
           with Not_found -> acc
         end
      | UCase swts -> swts_to_stites env nt sigma swts :: acc
    ) acc tr_assigns

let nondets_to_actions env _ acc =
  List.fold_left 
    (fun acc (h) ->
      let nt = Elem (h, Glob) in
      try (St_assign (HT.find env.id_terms nt, -1)) :: acc
      with Not_found -> acc
    ) acc

let update_to_actions procs sigma env acc
    {up_arr=a; up_arg=lj; up_swts=swts} =
  let indexes = Variable.all_arrangements_arity a procs in
  List.fold_left (fun acc li ->
    let sigma = (List.combine lj li) @ sigma in
    let at = Access (a, li) in
    swts_to_stites env at sigma swts :: acc
  ) acc indexes

let missing_reqs_to_actions env acct =
  List.fold_left (fun acc -> function
      | (a, Eq, b) ->
        (* variable on lhs *)
        let a, b =
          if not (is_variable env a) && is_variable env b then b, a
          else a, b in
        if List.exists
            (function St_assign (a', _) -> a = a' | _ -> false) acct
        then acc
        else (St_assign (a,b)) :: acc
      | _ -> acc) acct

let value_in_state env st i =
  if i <> -1 && i < env.nb_vars then st.(i) else i

let check_req env st (i1, op, i2) =
  let v1 = value_in_state env st i1 in
  let v2 = value_in_state env st i2 in
  v1 = -1 || v2 = -1 ||
    match op with
    | Eq -> v1 = v2
    | Neq -> v1 <> v2
    | Le -> v1 <= v2
    | Lt -> v1 < v2

let check_reqs env st = List.for_all (check_req env st)


let neg_req env = function
  | a, Eq, b ->
      if b = env.id_true then a, Eq, env.id_false
      else if b = env.id_false then a, Eq, env.id_true
      else a, Neq, b
  | a, Neq, b -> a, Eq, b
  | a, Le, b -> b, Lt, a
  | a, Lt, b -> b, Le, a


let rec print_action env fmt = function
  | St_ignore -> ()
  | St_arith (i, v, c) -> 
      fprintf fmt "%a + %d" Atom.print 
	(Atom.Comp (id_to_term env i, Eq, id_to_term env v)) c
  | St_assign (i, -1) -> 
      fprintf fmt "%a = ." Term.print (id_to_term env i)
  | St_assign (i, v) -> 
      fprintf fmt "%a" Atom.print 
	(Atom.Comp (id_to_term env i, Eq, id_to_term env v))
  | St_ite (l, a1, a2) ->
      fprintf fmt "ITE (";
      List.iter (fun (i, op, v) -> 
	eprintf "%a && " Atom.print 
	  (Atom.Comp (id_to_term env i, op, id_to_term env v))
      ) l;
      fprintf fmt ", %a , %a )" (print_action env) a1 (print_action env) a2

let rec apply_action env st sts' = function
  | St_assign (i1, i2) ->
    begin
      try
	let v2 = value_in_state env st i2 in
	List.iter (fun st' -> st'.(i1) <- v2) sts';
        sts'
      with Not_found -> sts'
    end
  | St_arith (i1, i2, c) when abstr_num ->
    begin
      try
	let v2 = value_in_state env st i2 in
	List.iter (fun st' -> st'.(i1) <- abstr_add env v2 c) sts';
        sts'
      with Not_found -> sts'
    end 
  | St_ite (reqs, a1, a2) -> (* explore both branches if possible *)
      let sts'1 = 
        if check_reqs env st reqs then 
          let sts' = List.map Array.copy sts' in 
          apply_action env st sts' a1
        else [] in
      let sts'2 =
        if List.exists (fun req -> check_req env st (neg_req env req)) reqs
        then 
          let sts' = List.map Array.copy sts' in
          apply_action env st sts' a2
        else [] in
      begin
        match sts'1, sts'2 with
          | [], [] -> sts'
          | _::_, [] -> sts'1
          | [], _::_ -> sts'2
          | _, _ -> List.rev_append sts'1 sts'2
      end
  | _ (* St_ignore or St_arith when ignoring nums *) -> sts'


let apply_actions env st acts =
  let st' = Array.copy st in
  List.fold_left (apply_action env st) [st'] acts



let print_transition_fun env name sigma { st_reqs = st_reqs;
                                          st_udnfs = st_udnfs;
                                          st_actions = st_actions } fmt =
  fprintf fmt "%a (%a)\n" Hstring.print name Variable.print_subst sigma;
  fprintf fmt "requires { \n";
      List.iter (fun (i, op, v) -> 
	fprintf fmt "          %a\n" Atom.print 
	  (Atom.Comp (id_to_term env i, op, id_to_term env v))
      ) st_reqs;
      List.iter (fun dnf ->
	fprintf fmt "          ";
	List.iter (fun r ->
	  List.iter (fun (i, op, v) -> 
	    fprintf fmt "%a &&" Atom.print 
	      (Atom.Comp (id_to_term env i, op, id_to_term env v))
	  ) r;
	  fprintf fmt " || ";
	) dnf;
	fprintf fmt "\n";
      ) st_udnfs;
      fprintf fmt "}\n";
      fprintf fmt "actions { \n";
      List.iter (fun a -> 
	fprintf fmt "         %a\n" (print_action env) a;
      ) st_actions;
      fprintf fmt "}\n@."


let compile_transition_instance procs env
    { tr_args; tr_reqs = reqs; tr_name = name; tr_ureq = ureqs;
       tr_assigns = assigns; tr_upds = upds; tr_nondets = nondets; _ }
    sigma =
  (* Substitute process variables in the guard *)
  let reqs = SAtom.subst sigma reqs in
  (* Get the effective process arguments after substitution *)
  let t_args_ef =
    List.fold_left (fun acc p ->
      try (Variable.subst sigma p) :: acc
      with Not_found -> p :: acc) [] tr_args in
  (* Compile universal guards to DNF form *)
  let udnfs = Forward.uguard_dnf sigma procs t_args_ef ureqs in
  (* Encode guards as integer requirements *)
  let st_reqs = satom_to_st_req env reqs in
  let st_udnfs = List.map (List.map (satom_to_st_req env)) udnfs in
  (* Compile all actions *)
  let st_actions = assigns_to_actions env sigma [] assigns in
  let st_actions = nondets_to_actions env sigma st_actions nondets in
  let st_actions = List.fold_left
    (update_to_actions procs sigma env)
    st_actions upds in
  (* Add frame actions for variables mentioned in guards but not assigned *)
  let st_actions = missing_reqs_to_actions env st_actions st_reqs in
  (* Build the executable closure *)
  let f = fun st ->
    (* Step 1: Check conjunctive guards (tr_reqs) *)
    if not (check_reqs env st st_reqs) then raise Not_applicable;
    (* Step 2: Check universal guards (tr_ureq) - each DNF needs one true disjunct *)
    if not (List.for_all (List.exists (check_reqs env st)) st_udnfs)
    then raise Not_applicable;
    (* Step 3: Apply all actions to produce successor state(s) *)
    apply_actions env st st_actions
  in
  let st_tr = {
    st_name = name;
    st_reqs = st_reqs;
    st_udnfs = st_udnfs;
    st_actions = st_actions;
    st_f = f;
  } in
  if debug then print_transition_fun env name sigma st_tr err_formatter;
  st_tr

let compose_st_f trs =
  let rec aux todo acc =
    match todo with
    | [] -> acc
    | (y,[])::todo -> aux todo (y::acc)
    | (y,(f::rest))::todo ->
      let after = try f.st_f y with Not_applicable -> [] in 
      if after = [] then raise Not_applicable;
      let todo = List.fold_left (fun acc z -> (z,rest)::acc) todo after in 
      aux todo acc in
  fun x -> aux [(x,trs)] []

(* Assumes we have enough procs *)
let compile_transaction_instance procs env calls sigma =
  let trs = ListLabels.rev_map calls ~f:(fun (tr,args) ->
      let s = ListLabels.fold_left2 tr.tr_info.tr_args args ~init:sigma
          ~f:(fun s formal actual -> (formal, Variable.subst s actual) :: s) in
      compile_transition_instance procs env tr.tr_info s
    ) in
  {
    st_name = Hstring.make "<transaction>";
    st_reqs = [];
    st_udnfs = [];
    st_actions = [];
    st_f = compose_st_f trs
  }

let transitions_to_func_aux procs env acc { tr_info; _ } =
  (* Skip if transition requires more processes than we have *)
  if List.length tr_info.tr_args > List.length procs then acc
  else
    (* Generate all possible instantiations of transition parameters *)
    let d = Variable.all_permutations tr_info.tr_args procs in
    (* Ensure at least one instantiation even for parameter-less transitions *)
    let d = if d = [] then [[]] else d in
    (* Compile each instantiation *)
    List.fold_left (fun acc sigma ->
      let st_tr = compile_transition_instance procs env tr_info sigma in
      st_tr :: acc
    ) acc d

let transactions_to_func_aux procs env acc (args, calls) =
  if List.length args > List.length procs then acc
  else
    (* Generate all possible instantiations of transition parameters *)
    let d = Variable.all_permutations args procs in
    (* Ensure at least one instantiation even for parameter-less transitions *)
    let d = if d = [] then [[]] else d in
    (* Compile each instantiation *)
    List.fold_left (fun acc sigma ->
        let st_tr = compile_transaction_instance procs env calls sigma in
        st_tr :: acc
      ) acc d


let transitions_to_func procs env = List.fold_left (transitions_to_func_aux procs env) []

let transaction_to_func procs env = List.fold_left (transactions_to_func_aux procs env) []

(******************************************************************************)
(*                  BFS EXPLORATION                                           *)
(*                                                                            *)
(* The main exploration loop. Starting from initial states, we iteratively    *)
(* apply all applicable transitions to discover reachable states.             *)
(******************************************************************************)
 
let post_bfs env st visited trs q cpt_q depth =
  if not limit_forward_depth || depth < forward_depth then
    (* Try each compiled transition *)
    List.iter (fun st_tr ->
        try
          (* Execute transition: checks guards, applies actions *)
          let sts = st_tr.st_f st in
          (* Add each successor to the queue if not already visited *)
          List.iter (fun s ->
            (* Optionally normalize for symmetry reduction *)
            if forward_sym then normalize_state env s;
            if not (HST.mem visited s) then begin
              HQueue.add ~cpt_q (depth + 1, s) q
            end
          ) sts
        with Not_applicable ->
          (* Guards not satisfied - transition doesn't fire from this state *)
          ()
    ) trs

(** Check if a candidate invariant is violated by a state.
    Returns [true] if the candidate is VIOLATED (i.e., the state is a
    counterexample), [false] if the state satisfies the candidate. *)
let check_cand env state cand =
  not (List.for_all (fun l -> check_req env state l) cand)


(** Main BFS exploration loop.
    Starting from initial states, explores reachable states level by level.
    All visited states are stored in [env.states] for later use in
    candidate checking (to disprove invariant candidates).

    @param env The environment with compiled transitions
    @param l  List of (depth, state) pairs for initial states *)
let forward_bfs _ _ env l =
  let h_visited = env.explicit_states in
  let cpt_f = ref 0 in  (* Count of explored states *)
  let cpt_r = ref 0 in  (* Count of registered states *)
  let cpt_q = ref 1 in  (* Queue size *)
  let trs = env.st_trs in  (* All compiled transitions *)
  let to_do = HQueue.create env.table_size in
  (* Initialize queue with initial states *)
  List.iter (fun td -> HQueue.add td to_do) l;
  (* BFS main loop *)
  while not (HQueue.is_empty to_do) &&
          (max_forward = -1 || !cpt_f < max_forward) do
    let depth, st = HQueue.take to_do in
    decr cpt_q;
    if not (HST.mem h_visited st) then begin
      (* Mark as visited *)
      HST.add h_visited st ();
      (* Compute successors and add to queue *)
      post_bfs env st h_visited trs to_do cpt_q depth;
      incr cpt_f;
      if debug && verbose > 1 then
        eprintf "%d : %a\n@." !cpt_f
          SAtom.print (state_to_cube env st);
      if not quiet && !cpt_f mod 1000 = 0 then
        eprintf "%d (%d)@." !cpt_f !cpt_q;
      incr cpt_r;
      (* Store state for later candidate checking *)
      env.states <- st :: env.states;
    end
  done

(************************************************************************)
(* Forward enumerative search, states are insterted in the global hash- *)
(* table explicit_states                                                *)
(************************************************************************)
let no_scan_states _env =
  (* Prevent the GC from scanning the list env.states as it is going to be
     kept in memory all the time. *)
  (* This is commented out for the moment as Obj.set_tag was removed in OCaml 5.00 *)
  (* List.iter (fun s -> Obj.set_tag (Obj.repr s) (Obj.no_scan_tag)) env.states *)
  ()

let finalize_search env =
  let st = HST.stats env.explicit_states in
  if not quiet then printf "Total forward nodes : %d@." st.Hashtbl.num_bindings;
  if verbose > 0 || profiling then begin
    printf "\n%a" Pretty.print_line ();
    printf "@{<b>Statistics@}\n";
    printf "----------\n";
    printf "Bindings         : %d@." st.Hashtbl.num_bindings;
    printf "Buckets          : %d@." st.Hashtbl.num_buckets;
    printf "Max bucket size  : %d@." st.Hashtbl.max_bucket_length;
    printf "Bucket histogram : @?";
    Array.iteri (fun i v -> if v <> 0 then printf "[%d->%d]" i v )
      st.Hashtbl.bucket_histogram;
    printf "@.";
  end;
  no_scan_states env;
  env.explicit_states <- HST.create 1;
  Gc.compact ();
  Gc.full_major ();
  TimeForward.pause ()


let install_sigint () =
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ ->
          printf "\n\n@{<b>@{<fg_red>ABORTING ENUMERATIVE!@}@} \
                  Received SIGINT@.";
          printf "Finalizing search.@.";
          raise Exit
       ))

let search procs init =
  TimeForward.start ();
  let procs = procs in
  (* Step 1: Initialize term-to-ID mappings *)
  let env = init_tables procs init in
  (* Step 2: Convert initial formula to concrete states *)
  let st_inits = init_to_states env procs init in
  if debug then
    List.iter (fun (_, st) ->
      eprintf "init : %a\n@." SAtom.print (state_to_cube env st))
      st_inits;
  (* Step 3: Compile transitions to executable closures *)
  let st_trs = if Options.tract then
      transaction_to_func procs env init.t_transactions
    else
      transitions_to_func procs env init.t_trans in
  let env = { env with st_trs } in
  (* Register environment for later candidate checking *)
  global_envs := env :: !global_envs;
  install_sigint ();
  (* Step 4: Run BFS exploration *)
  begin try
    forward_bfs init procs env st_inits;
    with Exit -> ()
  end ;
  finalize_search env
                        

let resume_search_from _ _ = assert false

let find_tr_funs env name =
  List.filter (fun tr -> Hstring.equal tr.st_name name) env.st_trs

let satom_to_cand env sa =
  SAtom.fold (fun a c -> match a with
    | Atom.Comp (t1, op, t2) -> 
        (HT.find env.id_terms t1, op, HT.find env.id_terms t2) :: c
    | _ -> raise Not_found)
    sa []


(** Raised when a candidate invariant survives checking against all states *)
exception Sustainable of Node.t list


(******************************************************************************)
(*                  CANDIDATE INVARIANT CHECKING                              *)
(*                                                                            *)
(* These functions check if candidate invariants (approximations) can be      *)
(* disproved by the finite model built during exploration.                    *)
(*                                                                            *)
(* The key insight: if a candidate is violated by ANY explored state, it      *)
(* cannot be a valid invariant of the full (infinite) system. This provides   *)
(* a fast way to filter out bad candidates before expensive SMT checking.     *)
(*                                                                            *)
(* A candidate that survives (is not disproved by any state) is "sustainable" *)
(* and is returned for further verification by the BRAB algorithm.            *)
(******************************************************************************)


let alpha_renamings env procs s =
  let d = List.rev (Variable.all_permutations (Node.variables s) procs) in
  (* keep list.rev in order for the first element of perm to be
     a normalized cube as we will keep this only one if none of
     perm can be disproved *)
  List.fold_left (fun p sigma ->
    let c = Cube.subst sigma s.cube in
    let s' = Node.create ~kind:Approx c in
    (satom_to_cand env (Node.litterals s'), s') :: p
  ) [] d


let one_step () = if nocolor then eprintf "#@?" else eprintf " @?"


let resist_on_trace_size progress_inc ls env =
  let procs = List.rev (List.tl (List.rev env.all_procs)) in
  let cands, too_big =
    List.fold_left (fun (acc, too_big) s ->
      if Node.dim s > env.model_cardinal then acc, s :: too_big
      else
	try (alpha_renamings env procs s) :: acc, too_big
	with Not_found -> acc, too_big
    ) ([], []) ls  in
  let cands = ref (List.rev cands) in
  let too_big = List.rev too_big in
  if !cands = [] then []
  else
    try
      if not quiet then eprintf "@{<fg_black_b>@{<i>";
                        (* will be forgotten by flushs *)
      let cpt = ref 0 in
      List.iter (fun st ->
        incr cpt;
        if not quiet && !cpt mod progress_inc = 0 then one_step ();
        cands := List.filter (fun p -> 
          List.for_all (fun (c, _) -> check_cand env st c) p
        ) !cands;
        if !cands = [] then raise Exit;
      ) env.states;
      let remain = List.fold_left (fun acc clas ->
	match clas with
	| [] -> acc
	| (_, s) :: _ -> s :: acc) [] !cands in
      List.rev_append remain too_big
    with 
      | Exit | Not_found -> too_big


let smallest_to_resist_on_trace ls =
  try
    let nb =
      List.fold_left (fun nb env -> nb + List.length env.states)
		     0 !global_envs
    in
    let progress_inc = nb / Pretty.vt_width + 1 in
    TimeCheckCand.start ();
    let resistants =
      List.fold_left
	(resist_on_trace_size progress_inc) ls !global_envs in
    match resistants with
      | s :: _ -> raise (Sustainable [s])
      | [] -> raise Not_found
  with
  | Exit | Not_found ->
     TimeCheckCand.pause ();
     if not quiet then eprintf "@{</i>@}@{<bg_default>@}@{<fg_red>X@}@.";
     []
  | Sustainable ls ->
     TimeCheckCand.pause ();
     if not quiet then eprintf "@{</i>@}@{<bg_default>@}@{<fg_green>!@}@.";
     ls



exception EBad of state * env
exception ECantSay


let state_impossible env st s =
    if Node.dim s > env.model_cardinal then true
    else
      try
        check_cand env st (satom_to_cand env (Node.litterals s))
      with 
      | Not_found -> false

let one_resist_on_trace_size s env =
    if Node.dim s <= env.model_cardinal then 
      try
        let procs = List.rev (List.tl (List.rev env.all_procs)) in
        let ls = alpha_renamings env procs s in
        List.iter (fun st ->
          if not (List.for_all (fun (c, _) -> check_cand env st c) ls) then
            raise (EBad (st, env));
        ) env.states;
      with 
      | Not_found -> raise ECantSay


let check_first_and_filter_rest = function
  | [] -> []
  | s :: rs ->
     try
       List.iter (one_resist_on_trace_size s) !global_envs;
       raise (Sustainable [s])
     with
     | ECantSay -> rs
     | EBad (st, env) ->
        List.filter (state_impossible env st) rs
     


let rec check_remaining_aux cpt progress_inc ls =
  if not quiet && cpt mod progress_inc = 0 then one_step ();
  match check_first_and_filter_rest ls with
  | [] -> []
  | rs -> check_remaining_aux (cpt+1) progress_inc rs

let check_remaining progress_inc ls = check_remaining_aux 0 progress_inc ls

let fast_resist_on_trace ls =
  let progress_inc = (List.length ls) / Pretty.vt_width + 1 in
  if not quiet then eprintf "@{<fg_black_b>@{<i>";
                            (* will be forgotten by flushs *)
  TimeCheckCand.start ();
  try
    assert (check_remaining progress_inc ls = []);
    TimeCheckCand.pause ();
    if not quiet then eprintf "@{</i>@}@{<bg_default>@}@{<fg_red>X@}@.";
    []
  with Sustainable cand ->
       TimeCheckCand.pause ();
       if not quiet then eprintf "@{</i>@}@{<bg_default>@}@{<fg_green>!@}@.";
       cand



(******************************************************)
(* TODO Extract unsat cores to find minimal candidate *)
(******************************************************)

(******************************************************************************)
(*                  PUBLIC INTERFACE                                          *)
(******************************************************************************)

let init system =
  set_liberal_gc ();
  let low = if brab_up_to then 0 else enumerative in
  for i = low to enumerative do
    let procs = Variable.give_procs i in
    if not quiet then
      Pretty.print_title std_formatter
        ("STATEFULL ENUMERATIVE FORWARD ["^(string_of_int i)^" procs]");
    search procs system;

    if not quiet then printf "%a@." Pretty.print_double_line ();
  done;
  reset_gc_params ()

let first_good_candidate candidates =
  match fast_resist_on_trace candidates with
  | c :: _ -> Some c
  | [] -> None

let mk_env nbprocs sys =
  (* create mappings but don't allocate hashtable *)
  let procs = Variable.give_procs nbprocs in
  let env = init_tables ~alloc:false procs sys in
  global_envs := env :: !global_envs;
  env

let int_of_term env t =
  (* if Term.type_of t == Smt.Type.type_int then *)
  HT.find env.id_terms t

let next_id env = env.pinf_int_abstr + 1

let empty_state = [||]

let new_undef_state env =
  Array.make env.nb_vars (-1)
  (* env.states <- st :: env.states; *)
  (* eprintf "nb states : %d@." (List.length env.states); *)
  (* st *)

let register_state env st = env.states <- st :: env.states

let size_of_env env = env.table_size

let print_last env =
  match env.states with
  | st :: _ -> eprintf "--- @[<hov>%a@]@." SAtom.print (state_to_cube env st)
  | _ -> ()
