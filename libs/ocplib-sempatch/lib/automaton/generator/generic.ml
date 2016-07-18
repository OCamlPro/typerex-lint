open Std_utils
open Parsetree
open Asttypes

module type ARG =
sig
  type result
  type middle

  val deriver_name : string

  val middle_of_record : string -> Parsetree.label_declaration list -> middle list
  val middle_of_variant : string -> Parsetree.constructor_declaration list -> middle list
  val middle_of_alias : string
    -> (Parsetree.type_declaration -> middle list)
    -> Parsetree.type_declaration list
    -> Parsetree.core_type
    -> middle list

  val middle_of_abstract : string -> middle

  val result_of_middle : middle list -> result
end

module type S =
sig
  type t

  val of_type_decl : env:Parsetree.type_declaration list
    -> Parsetree.type_declaration list
    -> t
end

module Make(Arg:ARG) : S with type t = Arg.result =
struct
  type t = Arg.result

  let fail = Common.raise_errorf

  let of_type_decl ~env type_decls =
    let rec middle_creator type_decl =
      let loc = type_decl.ptype_loc in
      let name = type_decl.ptype_name.txt in
      Ast_helper.with_default_loc loc @@ fun () ->
      match type_decl.ptype_kind with
      | Ptype_variant cases ->
        Arg.middle_of_variant name cases
      | Ptype_record fields ->
         Arg.middle_of_record name fields
      | Ptype_abstract ->
        begin
          match type_decl.ptype_manifest with
          | Some typ ->
            Arg.middle_of_alias name middle_creator env typ
          | None -> [Arg.middle_of_abstract name]
        end
      | Ptype_open ->
        fail "%s can't handle abstract types" Arg.deriver_name
    in
    Arg.result_of_middle @@ List.bind middle_creator type_decls
end
