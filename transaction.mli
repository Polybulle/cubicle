val is_ordinary : Ast.transition list -> bool
(** Every transition is an untriggered, yielding step without successor calls. *)

module type System = sig
  val it : Ast.transition list
end

module CFG_of (S : System) : sig
    val it : Ast.cfg
end
