open Ast
open Types

module Store = Cubetrie.Selected
module Fix = Fixpoint.FixpointTrie

let require b msg = if not b then failwith msg
let unsat f = try f (); false with Smt.Unsat _ -> true
let rejects f = try f (); false with Invalid_argument _ -> true
let pos t args = Before {evt_trans = Hstring.make t; evt_args = args}
let atom name v b = Atom.Comp (Access (Hstring.make name, [v]), Eq,
    Elem (Hstring.make (if b then "One" else "Zero"), Constr))
let cube vars atoms = Cube.create vars
    (List.fold_left (fun s a -> SAtom.add a s) SAtom.empty atoms)
let node state vars atoms = Node.create ~pos:state (cube vars atoms)
let store nodes = List.fold_left (fun s n -> Store.add_node n s) Store.empty nodes
let covered r = r <> None

let check_all label expected goal covers =
  let s = store covers in
  let norm = Node.normalize goal in
  List.iter (fun cover ->
    let cover = if Options.tx_bwd then Node.normalize cover else cover in
    let images instances = List.map (fun sigma ->
      List.map (Variable.subst sigma) (Node.variables cover)) instances in
    let expected = images (Instantiation.relevant_unnorm ~of_node:cover ~to_node:norm) in
    require (images (Instantiation.relevant ~of_node:cover ~to_node:norm) = expected)
      (label ^ ": normalized instantiation order");
    if Options.tx_bwd && norm.state <> Node.neutral_pos then
      require (images (Instantiation.exhaustive ~of_node:cover ~to_node:norm) = expected)
        (label ^ ": normalized exhaustive instantiation order")) covers;
  List.iter (fun (name, result) -> require (covered result = expected) (label ^ ": " ^ name))
    ["trie", Fix.check norm s; "hard", Fix.hard_fixpoint norm s;
     "naive", Fixpoint.FixpointTrieNaive.check norm s;
     "list", Fixpoint.FixpointList.check goal covers;
     "pure", Fixpoint.FixpointList.pure_smt_check goal covers];
  if not expected then begin
    require (Fix.easy_fixpoint norm s = None) (label ^ ": easy");
    require (Fix.peasy_fixpoint norm s = None) (label ^ ": permutation")
  end

(* Independent finite semantics: enumerate process assignments and Boolean arrays.
   No production normalization, substitution, trie or solver operations are used. *)
type pattern = {loc : string; args : int list; vars : int list;
                cells : (string * int * bool) list}
let rec assignments vars available = match vars with
  | [] -> [[]]
  | v :: rest -> List.concat_map (fun i ->
      List.map (fun xs -> (v, i) :: xs)
        (assignments rest (List.filter ((<>) i) available))) available
let holds population bits location args pat =
  pat.loc = location && List.exists (fun env ->
    List.map (fun v -> List.assoc v env) pat.args = args &&
    List.for_all (fun (a, v, b) ->
      let index = List.assoc v env + (if a = "A" then 0 else population) in
      ((bits land (1 lsl index)) <> 0) = b) pat.cells)
    (assignments pat.vars (List.init population (fun i -> i)))
let finite_cover population goal covers =
  let tuples = List.map (fun env -> List.map (fun v -> List.assoc v env) goal.args)
      (assignments goal.vars (List.init population (fun i -> i))) in
  List.for_all (fun args ->
    let rec valuations bits =
      bits = 1 lsl (2 * population) ||
      ((not (holds population bits goal.loc args goal) ||
        List.exists (holds population bits goal.loc args) covers) && valuations (bits + 1)) in
    valuations 0) tuples

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let p = List.nth Variable.procs 0 and q = List.nth Variable.procs 1 in
    let r = List.nth Variable.procs 2 and s = List.nth Variable.procs 3 in
    let a v b = atom "A" v b in
    let neutral = Node.neutral_pos in
    let goal = node neutral [q] [a q true] in
    let two = node neutral [p; q] [a p true; a q true] in
    check_all "gapped witnesses" false goal [two];
    let single = node neutral [p] [a p true] in
    require (Node.normalize single == single) "normalization copied canonical node";
    check_all "gapped alpha" true goal [single];
    let ordinary : Node.t Cubetrie.t =
      Cubetrie.Ordinary.add_node single Cubetrie.Ordinary.empty in
    require (Cubetrie.mem_array (Node.array single) ordinary <> None)
      "ordinary store is not the normal cube trie";
    let broad = node neutral [] [] in
    let selected = store [single; broad] in
    require (List.length (Store.all_vals selected) = 1)
      "wrong storage module selected";
    List.iter (fun instantiate ->
      List.iter (fun sigma -> require (Variable.well_formed_subst sigma)
        "noninjective instance") (instantiate ~of_node:two ~to_node:goal))
      [Instantiation.relevant; Instantiation.exhaustive];
    let eq x y = Atom.Comp (Elem (x, Var), Eq, Elem (y, Var)) in
    let bad = node neutral [q; s] [eq q s] in
    require (unsat (fun () -> Prover.assume_goal (Node.normalize bad))) "gapped goal distinctness";
    require (unsat (fun () -> Prover.assume_goal_nodes (Node.normalize bad) [])) "gapped multi-goal distinctness";
    let initial_bad = node neutral [s] [a s true] in
    require (unsat (fun () -> Prover.unsafe sys (Node.normalize initial_bad))) "gapped initial SMT";
    Safety.check sys initial_bad;
    let initial_good = node neutral [s] [a s false] in
    (try Safety.check sys initial_good; failwith "missed initial intersection"
     with Safety.Unsafe n -> require (n == initial_good) "lost original unsafe node");
    (try Safety.check ~normalized:(Node.normalize initial_good) sys initial_good;
         failwith "missed shared initial intersection"
     with Safety.Unsafe n -> require (n == initial_good) "shared query lost original unsafe node");
    require (Prover.make_formula (Node.array goal) == Prover.make_formula (Node.array goal))
      "lost formula cache";
    if Options.tx_bwd then begin
      let t = pos "t" [p] in
      let g = node t [p] [a p true] in
      require (Node.normalize g == g) "normalization copied canonical internal node";
      let other = node (pos "u" [p]) [p] [a p true] in
      let different = node (pos "t" [q]) [p; q] [a p true; a q false] in
      check_all "different constructor" false g [other];
      check_all "neutral not internal" false g [single];
      check_all "internal not neutral" false goal [g];
      check_all "different binding" false g [different];
      check_all "control alpha" true g [node (pos "t" [s]) [s] [a s true]];
      let union_yes = node t [p; q] [a p true; a q true] in
      let union_no = node t [p; q] [a p true; a q false] in
      check_all "extra cover alone yes" false g [union_yes];
      check_all "extra cover alone no" false g [union_no];
      check_all "extra cover union" true g [union_yes; union_no];
      (match Fix.hard_fixpoint g (store [union_yes; union_no]) with
       | Some tags -> require (Options.smt_solver = Options.Z3 ||
           (List.mem union_yes.tag tags && List.mem union_no.tag tags))
           "union core lost cover tags"
       | None -> assert false);
      let mixed = node (pos "zero" []) [p; q]
          [Atom.Comp (Access (Hstring.make "A", [p]), Neq, Access (Hstring.make "A", [q]))] in
      check_all "multiple instances of one cover" true mixed
        [node (pos "zero" []) [p] [a p true]];
      let loc_only = node (pos "pair" [s; q]) [q] [a q true] in
      let normalized = Node.normalize loc_only in
      require (Node.variables normalized = [p; q] && Node.variables loc_only = [q])
        "control-only support changed original data variables";
      require (normalized.state = pos "pair" [p; q]) "control-first tuple normalization";
      require (ArrayAtom.equal (Node.array normalized) (Node.array (node t [p; q] [a q true])))
        "control-first normalization did not rename data consistently";
      let active_zero = node (pos "t" [q]) [p; q] [a p true; a q false] in
      let active_one = node (pos "t" [p]) [p; q] [a p true; a q false] in
      require ((Node.normalize active_zero).state = (Node.normalize active_one).state)
        "control arguments are not canonical";
      check_all "control-first distinguishes active value" false active_zero [active_one];
      check_all "control-first reverse distinction" false active_one [active_zero];
      require (Node.normalize normalized == normalized) "normalization is not idempotent";
      let shared = Store.add_node ~normalized loc_only Store.empty in
      let discarded = node (pos "u" [q]) [q] [] in
      let shared = Store.add_node discarded shared in
      let shared = Store.delete (fun n -> n == discarded) shared in
      require (Store.all_vals shared = [loc_only] &&
               List.hd (Store.all_vals shared) == loc_only)
        "shared normalization replaced original stored node";
      require (Store.fold_at normalized (fun acc n -> n == normalized && acc) true shared)
        "cover traversal rebuilt the normalized node";
      require (Store.mem normalized shared <> None) "rebuilt index lost normalized data";
      let replacement = node loc_only.state [q] [a q true] in
      ignore (Store.delete_subsumed ~normalized:(Node.normalize replacement) replacement shared);
      require (loc_only.deleted && not normalized.deleted)
        "shared normalization did not preserve original deletion flag";
      loc_only.deleted <- false;
      require (unsat (fun () -> Prover.assume_goal_no_check normalized;
        Prover.SMT.assume ~id:0 (Prover.make_literal (eq p q)); Prover.run ()))
        "control-only variable not distinct";
      let swapped = node (pos "pair" [q; p]) [p; q] [a p true; a q false] in
      let renamed = node (pos "pair" [p; q]) [p; q] [a q true; a p false] in
      check_all "swap data and arguments" true swapped [renamed];
      check_all "do not swap arguments alone" false swapped
        [node (pos "pair" [p; q]) [p; q] [a p true; a q false]];
      check_all "raw covers with control-only and extra variables" true
        (node (pos "pair" [p; q]) [] [])
        [node (pos "pair" [s; q]) [r] [a r true];
         node (pos "pair" [s; q]) [r] [a r false]];
      require (Fix.easy_fixpoint union_yes (store [g]) <> None)
        "quick covering still partitions by support size";
      let compressed = store [union_yes; g] in
      require (Store.all_vals compressed = [g]) "cross-support compression failed";
      require (Fix.peasy_fixpoint (Node.normalize active_zero) (store [active_one]) = None)
        "quick permutation moved active participant";
      ignore (Store.delete_subsumed active_zero (store [active_one]));
      require (not active_one.deleted) "deletion permutation moved active participant";
      let free_goal = node t [p; q; r] [a p true; a q true; a r false] in
      let free_cover = node t [p; q; r] [a p true; a q false; a r true] in
      require (Fix.peasy_fixpoint free_goal (store [free_cover]) <> None)
        "quick permutation no longer renames free variables";
      ignore (Store.delete_subsumed free_goal (store [free_cover]));
      require free_cover.deleted "deletion no longer renames free variables";
      let broad = node t [] [] in
      let stored = store [g; other; different; loc_only; broad] in
      require (List.exists ((==) other) (Store.all_vals stored)) "insertion erased other location";
      require (not (List.exists ((==) different) (Store.all_vals stored)))
        "broad cube did not compress larger support";
      require (List.exists ((==) loc_only) (Store.all_vals stored)) "insertion erased other constructor";
      let counted = ref 0 in
      ignore (Store.delete_subsumed ~cpt:counted broad (store [g; other; different]));
      require (g.deleted && not other.deleted && different.deleted && !counted = 2)
        "located deletion or original identity";
      let tr = (List.hd sys.t_trans).tr_info in
      let ancestor = node t [p] [a p true] in
      let descendant = Node.create ~pos:t ~from:(Some (tr, [p], ancestor)) ancestor.cube in
      ignore (Store.delete_subsumed descendant (store [ancestor]));
      require (not ancestor.deleted) "deleted covering node's ancestor";
      let broad_child = Node.create ~pos:t ~from:(Some (tr, [p], ancestor)) (cube [] []) in
      let retained = Store.delete_subsumed broad_child (store [ancestor]) in
      let retained = Store.add_node broad_child retained in
      require (Store.all_vals retained = [broad_child] && not ancestor.deleted)
        "compression changed ancestor deletion flag";
      require (Node.origin broad_child == ancestor) "compression lost history";
      let retained = Store.delete (fun n -> n == broad_child) retained in
      require (Store.all_vals retained = []) "deleted trie entry remains stored";
      let other_ancestor = node t [p] [a p true] in
      let other_child = Node.create ~pos:t ~from:(Some (tr, [p], other_ancestor)) (cube [] []) in
      let retained = store [other_ancestor; other_child] in
      let replacement = node t [] [] in
      ignore (Store.delete_subsumed replacement retained);
      require (not other_ancestor.deleted && other_child.deleted)
        "deletion did not follow compressed trie contents";
      let child = Node.create ~pos:(pos "u" [p])
          ~from:(Some (tr, [p], ancestor)) ancestor.cube in
      let snapshot = store [ancestor; child] in
      ignore (Node.normalize child);
      require (Node.origin child == ancestor) "query rewrote history";
      ancestor.deleted <- true;
      let clean = Store.delete_subsumed broad snapshot in
      require (Store.all_vals clean = []) "cross-location descendant cleanup";
      let dummy = node Node.dummy_pos [] [] in
      require (rejects (fun () -> ignore (Node.normalize dummy)))
        "dummy goal accepted at normalization boundary";
      require (rejects (fun () -> ignore (store [dummy]))) "dummy stored";
      require (rejects (fun () -> ignore (Fixpoint.FixpointList.check broad [broad; dummy])))
        "list quick check skipped dummy validation";
      require (rejects (fun () -> ignore (Fixpoint.FixpointList.pure_smt_check broad [dummy])))
        "pure checker accepted dummy";
      require (rejects (fun () -> ignore (store [{broad with kind = Inv}])))
        "internal invariant accepted";
      require (rejects (fun () -> ignore (store [{broad with kind = Orig}])))
        "internal unsafe root accepted";
      require (rejects (fun () -> ignore (store [node
        (Before {evt_trans = Types.neutral_name; evt_args = [p]}) [] []])))
        "neutral arguments accepted";
      require (rejects (fun () -> ignore (Fixpoint.FixpointCertif.useful_instances broad [])))
        "transaction certificates enabled";
      require (rejects (fun () -> Safety.check sys broad)) "internal safety query";
      let shapes = [
        {loc="t"; args=[1]; vars=[1]; cells=[]};
        {loc="t"; args=[1]; vars=[1]; cells=["A",1,true]};
        {loc="t"; args=[2]; vars=[1;2]; cells=["A",1,true]};
        {loc="t"; args=[1]; vars=[1;2]; cells=["A",1,true;"A",2,true]};
        {loc="t"; args=[1]; vars=[1;2]; cells=["A",1,true;"A",2,false]};
        {loc="t"; args=[1]; vars=[1;2]; cells=["A",2,true;"B",1,false]};
        {loc="pair"; args=[1;2]; vars=[1;2]; cells=["A",1,true;"A",2,false]};
        {loc="pair"; args=[2;1]; vars=[1;2]; cells=["A",1,true;"A",2,false]};
        {loc="u"; args=[1]; vars=[1]; cells=["A",1,true]};
        {loc="zero"; args=[]; vars=[1]; cells=["A",1,true]};
      ] in
      let materialize pat =
        let var i = List.nth Variable.procs (2 * i - 1) in
        node (pos pat.loc (List.map var pat.args)) (List.map var pat.vars)
          (List.map (fun (a,v,b) -> atom a (var v) b) pat.cells) in
      let comparisons = ref 0 in
      List.iter (fun goal ->
        List.iter (fun c1 -> List.iter (fun c2 ->
          let expected = finite_cover 3 goal [c1; c2] in
          check_all "finite located semantics" expected (materialize goal)
            [materialize c1; materialize c2];
          incr comparisons) shapes) shapes) shapes;
      Printf.printf "PASS %d finite-semantics covering comparisons\n%!" !comparisons;
      let internal = List.map (fun state -> node state [p] [a p false])
          [pos "t" [p]; pos "t" [p]; pos "u" [p]] in
      let invariant = Node.create ~kind:Inv ~pos:neutral (cube [p] [a p false]) in
      (* These synthetic locations are not part of the parsed model's CFG. *)
      let cfg = {sys.cfg with
        should_check_fixpoint = (fun _ -> true);
        parent_calls_of = (fun _ -> []); final_calls = (fun _ -> [])} in
      (match Bwd.Selected.search ~candidates:internal
          {sys with cfg; t_unsafe = internal; t_invs = [invariant]} with
       | Bwd.Unsafe _ -> failwith "internal initial-state check"
       | Bwd.Safe (visited, candidates) ->
          require (candidates = []) "internal approximation selected";
          require (List.length (List.filter (fun n -> n.state <> neutral) visited) = 2)
            "scheduler failed to store/cover located nodes");
      Printf.printf "PASS scheduler storage and neutral-only policy\n%!"
    end else begin
      let dummy = node Node.dummy_pos [s] [a s true] in
      check_all "ordinary dummy invariant" true goal [dummy];
      let cert = Fixpoint.FixpointCertif.useful_instances goal [single] in
      require (cert <> [] && List.for_all (fun (n,sigma) -> n == single &&
        Variable.well_formed_subst sigma && Variable.subst sigma p = q) cert)
        "certificate did not restore original names";
      let gapped = node neutral [s] [a s true] in
      let cert = Fixpoint.FixpointCertif.useful_instances goal [gapped] in
      require (cert <> [] && List.for_all (fun (n, sigma) -> n == gapped &&
        List.map fst sigma = [s] && Variable.subst sigma s = q) cert)
        "certificate renamed source variables";
      let yes = node neutral [p; q] [a p true; a q true] in
      let no = node neutral [p; q] [a p true; a q false] in
      let cert = Fixpoint.FixpointCertif.useful_instances goal [yes; no] in
      require (Options.smt_solver = Options.Z3 ||
        (List.length cert >= 2 && List.for_all (fun (_, sigma) ->
        Variable.well_formed_subst sigma) cert))
        ("extra certificate names collided: " ^ String.concat "; "
           (List.map (fun (n, sigma) -> Format.asprintf "%d [%a]"
             n.tag Variable.print_subst sigma) cert))
    end;
    Printf.printf "PASS located fixpoint contracts\n%!"
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
