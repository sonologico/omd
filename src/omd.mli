(** A markdown parser in OCaml. *)

type attributes =
  (string * string) list

type 'a link =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type 'attr inline =
  | Concat of 'attr * 'attr inline list
  | Text of 'attr * string
  | Emph of 'attr * 'attr inline
  | Strong of 'attr * 'attr inline
  | Code of 'attr * string
  | Hard_break of 'attr
  | Soft_break of 'attr
  | Link of 'attr * 'attr inline link
  | Image of 'attr * 'attr inline link
  | Html of 'attr * string

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

type 'a def_elt =
  {
    term: 'a;
    defs: 'a list;
  }

type ('attr, 'a) base_block =
  | Paragraph of 'attr * 'a
  | List of 'attr * list_type * list_spacing * ('attr, 'a) base_block list list
  | Blockquote of 'attr * ('attr, 'a) base_block list
  | Thematic_break of 'attr
  | Heading of 'attr * int * 'a
  | Code_block of 'attr * string * string
  | Html_block of 'attr * string
  | Definition_list of 'attr * 'a def_elt list

type 'attr block = ('attr, 'attr inline) base_block

type doc = attributes block list
(** A markdown document *)

val of_channel: in_channel -> doc

val of_string: string -> doc

val to_html: doc -> string

val to_sexp: doc -> string
