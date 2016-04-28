type t = {
  current_match: Match.t;
  matches: (Match.t * Location.t) list;
}

val set_current: Match.t -> t -> t
val add_expr: string -> Parsetree.expression_desc -> t -> t
val add_ident: string -> string -> t -> t

val set_matches: (Match.t * Location.t) list -> t -> t

val get_expr: string -> t -> Parsetree.expression_desc option
val get_ident: string -> t -> string option

val empty : t

val merge: t -> t -> t
