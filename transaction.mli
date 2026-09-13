module type System = sig
  val it : Ast.transition list
end

module CFG_of (S : System) : sig
    val it : Ast.cfg
end
