(**************************************************)
(* Id of type                                     *)
(**************************************************)

val id_of_path : Path.t -> string
val id_of_typ_expr : Types.type_expr -> string option

(**************************************************)
(* Failure handling                               *)
(**************************************************)

val fail : ('a, unit, string, 'b) format4 -> 'a

(**************************************************)
(* Conversion between Types.t and Parsetree types *)
(**************************************************)

val longident_of_path : Path.t -> Longident.t
val core_type_of_type_expr : Types.type_expr -> Parsetree.core_type

(**************************************************)
(* Construction of longidents                     *)
(**************************************************)

module Pfx:
sig
  val cstr : Longident.t -> Longident.t

  val mk : string -> string -> Longident.t
  val t : string -> Longident.t
  val st : string -> Longident.t
  val n : string -> Longident.t
end

val (!!) : string -> string
val flatten : Longident.t -> Longident.t

(**************************************************)
(* Generation of arguments                        *)
(**************************************************)

val nth_arg : int -> string
val nth_state : int -> string

val gen_args : 'a list -> string list
val gen_states : 'a list -> string list

(**************************************************)
(* Searching in type environment                  *)
(**************************************************)

val get_type : string
  -> (string * Types.type_declaration) list
  -> Types.type_declaration option

val deriving : Parsetree.attributes

