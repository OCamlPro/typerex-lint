val id : string -> string
val cstr : string -> string
val print_longident : Longident.t -> string
val upprint : Parsetree.core_type -> string option

val mk_exploded : string -> Longident.t
val mk_aut : string -> Longident.t
val mk_aut_cstr : string -> Longident.t
val mk_match : string -> Longident.t

val warn : ('a, unit, string, unit) format4 -> 'a
val debug : ('a, unit, string, unit) format4 -> 'a
val raise_errorf : ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a

(** [instantiate_type_decl replacements typ] replaces all the free variables of
    typ present in the association list replacements by its corresponding value
*)
val instantiate_type_decl : (string * Parsetree.core_type) list
  -> Parsetree.type_declaration
  -> Parsetree.type_declaration

(** Returns the list of toplevel value-bindings in the given structure *)
val get_val_decls : Parsetree.structure -> Parsetree.value_binding list

(** [filter_decls type_decls] returns the list of monomorphic type
   declarations in type_decls *)
val filter_decls : Parsetree.type_declaration list
    -> Parsetree.type_declaration list

val stdlib : Parsetree.type_declaration list
val concrete_stdlib : Parsetree.type_declaration list
