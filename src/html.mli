type element_type =
  | Inline
  | Block

type t =
  | Element of element_type * string * Ast.attributes * t option
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

val of_doc : Ast.attributes Ast.t list -> t

val to_string : t -> string

val to_plain_text : t -> string
