type unfiltered_patch
type t

val ( ->> ) : t -> unfiltered_patch -> t
val ( >> ) : t list -> t -> t list

val filter : Ast_filter.t -> t
val filter_simple : Ast_filter.filter_leaf -> t

val rename_var : ?rename_def:bool -> string -> string -> unfiltered_patch
val add_arg_fun : string -> string -> unfiltered_patch
val make_fun_call : string -> Parsetree.expression -> unfiltered_patch
val insert_at_toplevel: (?loc:Location.t -> unit -> Parsetree.structure_item) -> unfiltered_patch
val insert_open : string -> unfiltered_patch

val cst : Asttypes.constant -> Parsetree.expression

val register : string -> t list -> unit
val run_main : t list -> unit
