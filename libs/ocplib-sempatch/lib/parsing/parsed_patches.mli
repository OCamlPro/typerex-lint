type header = {
  meta_expr : string list;
  name : string;
  guard : Guard.t list;
  keyvals : string SUStringMap.t;
}

type body = Automaton.A.state

type patch = {
  header: header;
  body: body;
}

type unprocessed_header = header
type unprocessed_body = Parsetree.expression
type unprocessed_patch = {
  unprocessed_header : unprocessed_header;
  unprocessed_body : unprocessed_body;
}

type setting =
  | Expressions of string list
  | KeyVal of string * string
  | Name of string
  | Guard of Guard.t

type t = patch

val get_name : t -> string
val get_msg : t -> string option
val get_metavariables : t -> string list
val get_guard : t -> Guard.t list

val get_body : t -> body

val void_header : header
val header_from_list : setting list -> header

val preprocess : unprocessed_patch -> t
