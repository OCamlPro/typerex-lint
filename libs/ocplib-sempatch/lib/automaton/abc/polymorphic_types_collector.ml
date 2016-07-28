open Std_utils
open Types

module C = Abc_common

let rec from_signature_item = function
  | Sig_type (_, decl, _) -> from_type_declaration decl
  | _ -> []

and from_type_declaration { type_kind; type_manifest; _ } =
  match type_kind with
  | Type_record (labels, _) ->
    List.bind from_label_declaration labels
  | Type_variant constrs ->
    List.bind from_constructor_declaration constrs
  | _ ->
    begin match type_manifest with
      | Some { desc = Ttuple types; _ } ->
        List.bind from_type_expr types
      | _ -> []
    end

and from_label_declaration { ld_type;  _} =
  from_type_expr ld_type

and from_constructor_declaration { cd_args; cd_res; _} =
  Option.(value [] @@ map from_type_expr cd_res) @
  List.bind from_type_expr cd_args

and from_type_expr texpr =
  match texpr.desc with
  | Tconstr (_, _::_, _)
  | Ttuple _
    ->
    begin
      match C.id_of_typ_expr texpr with
      | Some name -> [(name, texpr)]
      | None ->
        Messages.warn
          "Warning : can't derive an identifier from a type";
        []
    end
  | _ -> []

and from_signature sign =
  List.bind from_signature_item sign
  |> List.sort_uniq (Fun.compose_binop String.compare fst)
  |> List.map (fun (name, expr) ->
      name,
      {
        Types.type_kind = Types.Type_abstract;
        type_arity = 0;
        type_params = [];
        type_private = Asttypes.Public;
        type_manifest = Some expr;
        type_variance = [];
        type_newtype_level = None;
        type_loc = Location.none;
        type_attributes = [];
      }
    )
