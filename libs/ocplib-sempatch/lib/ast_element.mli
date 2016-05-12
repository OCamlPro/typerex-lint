type t =
  | Expression of Parsetree.expression
  | String of string
  | Pattern of Parsetree.pattern

val to_string : t -> string
