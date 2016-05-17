type t =
  | Expression of Parsetree.expression
  | Expression_opt of Parsetree.expression option
  | String of string
  | Pattern of Parsetree.pattern
  | Value_binding of Parsetree.value_binding
  | Structure_item of Parsetree.structure_item
  | Structure of Parsetree.structure

val to_string : t -> string

val from_expr : Parsetree.expression -> t
