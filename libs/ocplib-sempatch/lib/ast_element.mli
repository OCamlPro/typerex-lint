type t =
  | Expression of Parsetree.expression
  | Ident of string

val to_string : t -> string
