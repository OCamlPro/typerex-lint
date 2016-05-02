type t = {
  current_match: Substitution.t;
  matches: Match.t list;
}

val set_current: Substitution.t -> t -> t
val add_expr: string -> Parsetree.expression -> t -> t
val add_ident: string -> string -> t -> t

val set_matches: Match.t list -> t -> t

val get_expr: string -> t -> Parsetree.expression option
val get_ident: string -> t -> string option

val empty : t

val merge: t -> t -> t
