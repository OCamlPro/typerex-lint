module T = Types

module H = Ast_helper
module L = Location
module C = Abc_common

module Pfx = C.Pfx

type t = string option -> Parsetree.structure_item

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
  | T.Tlink expr -> to_core_type expr
  | _ -> assert false (* Don't want to handle the rest *)

let name = "Tree converter"

let middle_of_record type_name type_params fields =
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
    ~params:(List.map (fun t -> to_core_type t, Asttypes.Invariant)
               type_params)
    (Location.mknoloc type_name)

let middle_of_variant name type_params constructors =
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
    ~params:(List.map (fun t -> to_core_type t, Asttypes.Invariant)
               type_params)
    (Location.mknoloc name)

let middle_of_abstract type_params name =
  H.Type.mk
    ~params:(List.map (fun t -> to_core_type t, Asttypes.Invariant)
               type_params)
    (Location.mknoloc name)

let middle_of_alias name type_params value =
  H.Type.mk
    ~manifest:(to_core_type value)
    ~params:(List.map (fun t -> to_core_type t, Asttypes.Invariant)
               type_params)
    (Location.mknoloc name)

let result_of_middle typedefs prefix =
  let alias_typedefs =
    match prefix with
    | Some pfx ->
      List.map (fun td ->
          { td with
            Parsetree.ptype_manifest =
              Some (
                H.Typ.constr
                  (Location.mknoloc
                   @@ Pfx.mk pfx td.Parsetree.ptype_name.Asttypes.txt)
                  (List.map fst td.Parsetree.ptype_params));
          }
        )
        typedefs
    | None -> typedefs
  in
  H.Str.type_ alias_typedefs

let fail = Printf.ksprintf failwith

let of_type_decl ~env type_decls =
  let open Types in
  ignore env;
  let middle_creator (name, type_decl) =
    let loc = type_decl.type_loc in
    let name = name in
    Ast_helper.with_default_loc loc @@ fun () ->
    match type_decl.type_kind with
    | Type_variant cases ->
      middle_of_variant name type_decl.type_params cases
    | Type_record (fields, _) ->
      middle_of_record name type_decl.type_params fields
    | Type_abstract ->
      begin
        match type_decl.type_manifest with
        | Some typ ->
          middle_of_alias name type_decl.type_params typ
        | None -> middle_of_abstract type_decl.type_params name
      end
    | Type_open ->
      fail "%s can't handle abstract types" name
  in
  result_of_middle @@ List.map middle_creator type_decls
