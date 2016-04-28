open Std_utils

type t = Variable.t StringMap.t

val empty: t

val get: string -> t -> Variable.t option
val get_expr: string -> t -> Parsetree.expression_desc option
val get_ident: string -> t -> string option

val add_expr: string -> Parsetree.expression_desc -> t -> t
val add_ident: string -> string -> t -> t

val merge: t -> t -> t
