type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

let same_block_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Bullet c1, Bullet c2 -> c1 = c2
  | _ -> false

type 'a def_elt =
  {
    term: 'a;
    defs: 'a list;
  }

type ('a, 'attr) block =
  | Paragraph of 'attr * 'a
  | List of 'attr * list_type * list_spacing * ('a, 'attr) block list list
  | Blockquote of 'attr * ('a, 'attr) block list
  | Thematic_break of 'attr
  | Heading of 'attr * int * 'a
  | Code_block of 'attr * string * string
  | Html_block of 'attr * string
  | Definition_list of 'attr * 'a def_elt list

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


type attributes =
  (string * string) list

type raw = (string, attributes) block
type t = (attributes inline, attributes) block

let rec map (f : 'a -> 'b) : ('a, 'attr) block -> ('b, 'attr) block = function
| Paragraph (attr, x) -> Paragraph (attr, f x)
| List (attr, ty, sp, bl) ->
    List (attr, ty, sp, List.map (List.map (map f)) bl)
| Blockquote (attr, xs) ->
    Blockquote (attr, List.map (map f) xs)
| Thematic_break attr ->
    Thematic_break attr
| Heading (attr, level, text) ->
    Heading (attr, level, f text)
| Definition_list (attr, l) ->
    let f {term; defs} = {term = f term; defs = List.map f defs} in
    Definition_list (attr, List.map f l)
| Code_block (attr, label, code) ->
    Code_block (attr, label, code)
| Html_block (attr, x) ->
    Html_block (attr, x)
