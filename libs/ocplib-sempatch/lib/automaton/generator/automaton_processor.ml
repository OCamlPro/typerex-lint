open Std_utils
open Parsetree
open Asttypes

module M = Ast_mapper
module H = Ast_helper
module L = Location

let state_lid = Longident.Lident "state"

let remove_arguments = {
  M.default_mapper with
  M.typ =
    (fun self typ ->
       let desc = match typ.ptyp_desc with
         | Ptyp_constr (id, _) ->
           Ptyp_constr (id, [])
         | d -> d
       in
       M.(default_mapper.typ self { typ with ptyp_desc = desc })
    );
  type_declaration = (fun self typ ->
      let updated = {
        typ with
        ptype_params = [];
      }
      in
      M.(default_mapper.type_declaration self updated)
    );
}

let convert_to_states =
  {
    M.default_mapper with
    M.typ =
      (fun self typ ->
         let desc =
           match typ.ptyp_desc with
           | Ptyp_constr ({loc; txt = _}, _) ->
             let new_id = {loc; txt = state_lid;}
             in
             Ptyp_constr (new_id, [])
           | Ptyp_var _ ->
             Ptyp_constr (
               {loc = Location.none; txt = state_lid; },
               []
             )
           | d -> d
         in
         M.(default_mapper.typ self { typ with ptyp_desc = desc })
      );

    (* Convert abstract type declaration too *)
    type_declaration = (fun self typ ->
        let mapped = M.(default_mapper.type_declaration self typ) in
        let manifest =
          if mapped.ptype_kind = Ptype_abstract &&
             Option.is_none mapped.ptype_manifest
          then
            let loc = mapped.ptype_loc in
            Some (H.Typ.constr ~loc (L.mkloc state_lid loc) [])
          else
            mapped.ptype_manifest
        in
        {
          mapped with
          ptype_manifest = manifest;
        }
      );
  }

let apply_to_tuples mapper_to_apply = {
  M.default_mapper with
  M.typ = (fun self typ ->
      match typ.ptyp_desc with
      | Ptyp_tuple _ ->
        mapper_to_apply.M.typ mapper_to_apply typ
      | _ ->
        M.default_mapper.M.typ self typ
    );
}
