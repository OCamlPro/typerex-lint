type t = Ast_filter.t * Ast_mapper.mapper list

val ( ->> ) : t -> Ast_mapper.mapper -> t
val ( >> ) : t list -> t -> t list

val filter : Ast_filter.t -> t
val filter_simple : Ast_filter.filter_leaf -> t

val rename_var : ?rename_def:bool -> string -> string -> Ast_mapper.mapper
val add_arg_fun : string -> string -> Ast_mapper.mapper
val make_fun_call : string -> Parsetree.expression -> Ast_mapper.mapper
val insert_at_structure_toplevel: (?loc:Location.t -> unit -> Parsetree.structure_item) -> Ast_mapper.mapper
val insert_open : string -> Ast_mapper.mapper

val register : string -> t list -> unit
val run_main : t list -> unit
