type t =
  | Expression of Parsetree.expression
  | String of string
  | Pattern of Parsetree.pattern
  | Value_binding of Parsetree.value_binding

val to_string : t -> string

val from_expr : Parsetree.expression -> t
