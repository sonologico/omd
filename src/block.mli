module Pre : sig

  val of_channel: in_channel -> Ast.attributes Ast.raw list * Ast.attributes Ast.link_def list
  val of_string: string -> Ast.attributes Ast.raw list * Ast.attributes Ast.link_def list
end
