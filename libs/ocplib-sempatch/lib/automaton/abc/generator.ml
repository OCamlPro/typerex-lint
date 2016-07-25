open Std_utils
open Types

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

module Make(Arg:ARG) : S with type t = Arg.result =
struct
  type t = Arg.result

  let fail = Printf.ksprintf failwith

  let of_type_decl ~env type_decls =
    let rec middle_creator (name, type_decl) =
      let loc = type_decl.type_loc in
      let name = name in
      Ast_helper.with_default_loc loc @@ fun () ->
      match type_decl.type_kind with
      | Type_variant cases ->
        Arg.middle_of_variant name cases
      | Type_record (fields, _) ->
        Arg.middle_of_record name fields
      | Type_abstract ->
        begin
          match type_decl.type_manifest with
          | Some typ ->
            Arg.middle_of_alias name middle_creator env typ
          | None -> Arg.middle_of_abstract name
        end
      | Type_open ->
        fail "%s can't handle abstract types" Arg.name
    in
    Arg.result_of_middle @@ List.map middle_creator type_decls
end
