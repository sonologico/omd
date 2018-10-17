module List_kind : sig
  type t =
    | Ordered
    | Unordered
end

type 'a t =
  | Paragraph of 'a
  | List of List_kind.t * 'a t list list
  | Blockquote of 'a t list
  | Thematic_break
  | Atx_heading of int * 'a
  | Code_block of string * string
  | Html_block of string

val map: f:('a -> 'b) -> 'a t -> 'b t

module Parser : sig
  type blocks
  type t

  val empty : t
  val process : t -> string -> t
  val finish : t -> blocks
end with type blocks := string t list

val to_html : ('a -> string) -> 'a t -> string

val of_channel : in_channel -> string t list

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
