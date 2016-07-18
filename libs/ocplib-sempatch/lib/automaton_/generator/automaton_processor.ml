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
           let id = {
             id with txt = Longident.Lident
                         (Common.id @@ Common.print_longident id.txt)
           }
           in
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

    (* Convert abstract type declaration too and remove manifest if there is
       another definition *)
    type_declaration = (fun self typ ->
        let mapped = M.(default_mapper.type_declaration self typ) in
        let manifest =
          match mapped.ptype_kind with
          | Ptype_abstract when Option.is_none mapped.ptype_manifest ->
            let loc = mapped.ptype_loc in
            Some (H.Typ.constr ~loc (L.mkloc state_lid loc) [])
          | Ptype_record _ | Ptype_variant _ | Ptype_open -> None
          | _ -> mapped.ptype_manifest
        in
        {
          mapped with
          ptype_manifest = manifest;
        }
      );
    (* Replace constructors with one tuple as argument with state *)
    (* FIXME : this is horribly ad-hoc *)
    type_kind = (fun self kind ->
        let mapped = M.(default_mapper.type_kind self kind) in
        match mapped with
        | Ptype_variant args ->
          Ptype_variant (List.map (
              fun cd ->
                let args =
                  match cd.pcd_args with
                  | [{ ptyp_desc = Ptyp_tuple _; ptyp_loc = loc; _} as arg] ->
                    [
                      {
                        arg with
                        ptyp_desc = Ptyp_constr (L.mkloc state_lid loc, []);
                      }
                    ]
                  | args -> args
                in { cd with pcd_args = args }
            )
              args
            )
        | k -> k
      )
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
