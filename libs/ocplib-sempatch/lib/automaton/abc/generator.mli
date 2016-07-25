module type ARG =
sig
  type result
  type middle

  val name : string

  val middle_of_record : string -> Types.label_declaration list -> middle
  val middle_of_variant : string -> Types.constructor_declaration list -> middle
  val middle_of_alias : string
    -> ((string * Types.type_declaration) -> middle)
    -> (string * Types.type_declaration) list
    -> Types.type_expr
    -> middle

  val middle_of_abstract : string -> middle

  val result_of_middle : middle list -> result
end

module type S =
sig
  type t

  val of_type_decl : env:(string * Types.type_declaration) list
    -> (string * Types.type_declaration) list
    -> t
end

module Make(Arg:ARG) : S with type t = Arg.result
