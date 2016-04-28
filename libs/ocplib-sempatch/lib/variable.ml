(* module P = Parsetree *)

type t =
  | Expression of Parsetree.expression_desc
  | Ident of string
