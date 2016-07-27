module T = Types

module H = Ast_helper
module L = Location
module C = Abc_common

let rec to_core_type type_expr =
  match type_expr.T.desc with
  | T.Tconstr (path, args, _) ->
    H.Typ.constr
      (L.mknoloc @@ C.longident_of_path path)
      (List.map to_core_type args)
  | T.Ttuple args ->
    H.Typ.tuple (List.map to_core_type args)
  | T.Tvar (Some name) ->
    H.Typ.var name
  | _ -> assert false (* Don't want to handle the rest *)

module Arg : Generator.ARG with type result = Parsetree.structure_item =
struct
  type result = Parsetree.structure_item
  type middle = Parsetree.type_declaration

  let name = "Tree converter"

  let middle_of_record type_name fields =
    let fields =
      List.map
        (
          fun field ->
            H.Type.field
              (Location.mknoloc field.Types.ld_id.Ident.name)
              (to_core_type field.Types.ld_type)
        )
        fields
    in
    H.Type.mk
      ~kind:(Parsetree.Ptype_record fields)
      (Location.mknoloc type_name)

  let middle_of_variant name constructors =
    let constructors =
      List.map
        (
          fun constructor ->
            H.Type.constructor
              ~args:(List.map to_core_type constructor.Types.cd_args)
              (Location.mknoloc constructor.Types.cd_id.Ident.name)
        )
        constructors
    in
    H.Type.mk
      ~kind:(Parsetree.Ptype_variant constructors)
      (Location.mknoloc name)

  let middle_of_abstract name =
    H.Type.mk
      (Location.mknoloc name)

  let middle_of_alias name _rec _env value =
    H.Type.mk
      ~manifest:(to_core_type value)
      (Location.mknoloc name)

  let result_of_middle typedefs =
    H.Str.type_ typedefs
end

include Generator.Make (Arg)
