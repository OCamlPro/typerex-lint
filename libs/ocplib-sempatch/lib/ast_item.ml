open Parsetree

type _ t =
  | Expression : expression -> expression t
  | Pattern : pattern -> pattern t
