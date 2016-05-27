open Std_utils

type t = Ast_element.t StringMap.t

val empty: t

val get: string -> t -> Ast_element.t option
val get_expr: string -> t -> Parsetree.expression option
val get_ident: string -> t -> string option

val add_expr: string -> Parsetree.expression -> t -> t
val add_ident: string -> string -> t -> t
val add_pattern: string -> Parsetree.pattern -> t -> t

val to_list : t -> (string * Ast_element.t) list

val merge: t -> t -> t
