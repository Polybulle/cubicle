open Ast
open Types

let require b message = if not b then failwith message
let unsat f = try f (); false with Smt.Unsat _ -> true
let atom a value = Atom.Comp (Access (Hstring.make "A", [a]), Eq,
                             Elem (Hstring.make value, Constr))
let cube vars atoms = Cube.create vars (List.fold_left
    (fun acc a -> SAtom.add a acc) SAtom.empty atoms)
let node c = Node.create ~pos:Node.neutral_pos c
let pos args = Before {evt_trans = Hstring.make "touch"; evt_args = args}

let check_scope n =
  let Before evt = n.state in
  let scope = List.fold_left (fun acc v -> Variable.Set.add v acc)
      (SAtom.variables_proc (Node.litterals n)) evt.evt_args in
  require (Node.variables n = Variable.Set.elements scope &&
           Node.variables n = Variable.give_procs (Variable.Set.cardinal scope))
    "node variables are not the complete normalized scope"

let () =
  try
    let sys = Typing.system (Parser.system Lexer.token (Lexing.from_channel Options.cin)) in
    let p = List.nth Variable.procs 0 and q = List.nth Variable.procs 1 in
    let r = List.nth Variable.procs 2 and s = List.nth Variable.procs 3 in
    let rec subsets = function
      | [] -> [[]]
      | x :: xs -> let rest = subsets xs in rest @ List.map (List.cons x) rest in
    let supports = subsets [p; q; r; s] in
    List.iter (fun data -> List.iter (fun control ->
      let args = List.rev control @ control in
      let c = cube [p; q; r; s] (List.map (fun v -> atom v "One") data) in
      let n = Node.create ~pos:(pos args) c in
      check_scope n;
      let scope = List.filter (fun v -> List.mem v data || List.mem v control)
          [p; q; r; s] in
      let sigma = Variable.build_subst scope Variable.procs in
      let Before evt = n.state in
      require (evt.evt_args = List.map (Variable.subst sigma) args &&
               ArrayAtom.equal (Node.array n) (ArrayAtom.apply_subst sigma c.Cube.array))
        "data/control sharing or argument order changed";
      let again = Node.create ~pos:n.state n.cube in
      require (again.cube == n.cube && again.state == n.state)
        "normalized construction rebuilt formula or location";
      let released = node n.cube in
      check_scope released;
      require (Node.dim released = List.length data)
        "location release retained control-only variables";
      let goal = node (cube data (List.map (fun v -> atom v "One") data)) in
      let cover = node (cube control (List.map (fun v -> atom v "One") control)) in
      List.iter (fun instantiate ->
        let substs = instantiate ~of_cube:cover.cube ~to_cube:goal.cube in
        require (substs <> []) "lost all instances";
        List.iter (fun sigma ->
          require (List.length sigma = Node.dim cover &&
                   Variable.well_formed_subst sigma) "noninjective or partial instance") substs)
        [Instantiation.relevant; Instantiation.exhaustive]
    ) supports) supports;
    let many = Variable.give_procs 12 in
    require (List.length many = 12) "run this test with -max-procs 16";
    let many_node = Node.create ~pos:(pos many) (cube [] []) in
    check_scope many_node;
    require ((Node.create ~pos:many_node.state many_node.cube).cube == many_node.cube)
      "normalization is not stable past #9";
    require (Variable.extra_procs [p; q] [p; q] = [r; s]) "prefix allocation";
    require (Variable.extra_vars [p; q] [p] = [q]) "prefix extension";
    let goal = node (cube [q] [atom q "One"]) in
    let cover = node (cube [p; q] [atom p "One"; atom q "One"]) in
    let trie = Cubetrie.add_node cover Cubetrie.empty in
    List.iter (fun check -> require (check goal trie = None) "collapsed cover")
      [Fixpoint.FixpointTrie.check; Fixpoint.FixpointTrieNaive.check];
    require (Fixpoint.FixpointList.check goal [cover] = None) "collapsed list cover";
    require (Fixpoint.FixpointList.pure_smt_check goal [cover] = None)
      "collapsed pure SMT cover";
    let single = node (cube [p] [atom p "One"]) in
    require (Fixpoint.FixpointTrie.check goal (Cubetrie.add_node single Cubetrie.empty)
             <> None) "lost valid cover";
    let cert = Fixpoint.FixpointCertif.useful_instances goal [single] in
    require (cert <> [] && List.for_all (fun (_, sigma) ->
      Variable.well_formed_subst sigma && Variable.subst sigma p = p) cert)
      "certificate does not refer to normalized goal";
    let eq x y = Atom.Comp (Elem (x, Var), Eq, Elem (y, Var)) in
    let bad = node (cube [q; s] [eq q s]) in
    require (unsat (fun () -> Prover.assume_goal bad)) "normalized goal distinctness";
    require (unsat (fun () -> Prover.assume_goal_nodes bad [])) "naive distinctness";
    require (Prover.make_formula (Node.array goal) == Prover.make_formula (Node.array goal))
      "formula cache was bypassed";
    let tr = (List.hd sys.t_trans).tr_info in
    let located = Node.create ~pos:(pos [s; q; s])
        ~from:(Some (tr, [s], goal)) (cube [q] [atom q "One"]) in
    check_scope located;
    let Before evt = located.state in
    require (Node.variables located = [p; q] && evt.evt_args = [q; p; q] &&
             SAtom.mem (atom p "One") (Node.litterals located))
      "joint data/control normalization";
    require (unsat (fun () ->
      Prover.assume_goal_no_check located;
      Prover.SMT.assume ~id:located.tag (Prover.make_literal (eq p q));
      Prover.run ())) "control-only variable lost distinctness";
    (match located.from with
     | [(actual_tr, args, parent)] ->
        require (actual_tr == tr && args = [s] && parent == goal)
          "constructor renamed history or copied ancestor"
     | _ -> failwith "constructor changed history length");
    require (located.depth = 1 && Node.origin located == goal)
      "constructor lost history metadata";
    if Options.tx_bwd then begin
      let release_sys = {sys with cfg = {sys.cfg with parent_calls_of = (fun _ ->
          [{evt_trans = Types.neutral_name; evt_args = []}])}} in
      let released, postponed = Pre.pre_image release_sys located in
      require (postponed = [] && List.length released = 1) "boundary crossing split";
      let released = List.hd released in
      check_scope released;
      require (Node.dim released = 1 && released.state = Node.neutral_pos &&
               released.from == located.from && released.depth = located.depth &&
               released.kind = located.kind && released.tag = located.tag + 1)
        "boundary crossing lost metadata or retained control scope";
    end;
    let initial_bad = node (cube [s] [atom s "One"]) in
    require (unsat (fun () -> Prover.unsafe sys initial_bad)) "initial SMT check";
    Safety.check sys initial_bad;
    let initial_good = node (cube [s] [atom s "Zero"]) in
    (try Safety.check sys initial_good; failwith "missed initial intersection"
     with Safety.Unsafe n -> require (n == initial_good) "lost original unsafe node");
    (* Inspect actual predecessors, not only direct constructors. *)
    let generated = ref 0 in
    let rec explore depth n =
      check_scope n;
      if depth > 0 then begin
        let ls, post = Pre.pre_image sys n in
        List.iter (fun next ->
          incr generated;
          if not Options.tx_bwd then
            require (next.state = Node.neutral_pos) "ordinary node has active control";
          explore (depth - 1) next) (ls @ post)
      end in
    List.iter (explore 4) sys.t_unsafe;
    require (!generated > 0) "predecessor check did not exercise production path";
    Printf.printf "PASS normalized variable contracts (256 scope pairs)\n%!"
  with exn -> Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn); exit 1
