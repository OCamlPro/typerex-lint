open Parsetree
module M = Ast_mapper
module H = Ast_helper


let escape string =
  let rp s = Str.global_replace (Str.regexp_string s) in
  string
  |> rp " " "_"
  |> rp "(" "LPAR_"
  |> rp ")" "_RPAR"
  |> rp "*" "_AND_"
  |> rp "." "_DOT_"
  |> String.capitalize

let typ_collector met_types = {
  M.default_mapper with
  M.typ = (fun self typ ->
      begin
        match typ.ptyp_desc with
        | Ptyp_constr _
        | Ptyp_tuple _
          ->
          let name =
            let () = Pprintast.core_type Format.str_formatter typ in
            Format.flush_str_formatter ()
          in
          met_types := {
            ptype_name = Location.mknoloc name;
            ptype_params = [];
            ptype_cstrs = [];
            ptype_kind = Ptype_abstract;
            ptype_private = Asttypes.Public;
            ptype_manifest = Some typ;
            ptype_attributes = [];
            ptype_loc = typ.ptyp_loc;
          } :: !met_types
        | _ -> ()
      end;
      M.default_mapper.M.typ self typ
    );
}

let collect_types file =
  let types_list = ref [] in
  let in_chan = open_in file in
  let parsed = Parser.interface Lexer.token (Lexing.from_channel in_chan) in
  let collector = typ_collector types_list in
  ignore (collector.M.signature collector parsed);
  List.sort_uniq (fun t1 t2 ->
      String.compare
        t1.ptype_name.Asttypes.txt
        t2.ptype_name.Asttypes.txt
    )
    !types_list
  |> List.filter (fun t -> t.ptype_params = [])

let build_sum loc types =
  let sum_members =
    List.map (fun typ ->
        let typ_name = typ.ptype_name.Asttypes.txt in
        {
          pcd_name = Location.mkloc (escape typ_name) loc;
          pcd_args = [
            match typ.ptype_manifest with
            | Some t -> t
            | None -> {
              ptyp_desc =
                (Ptyp_constr
                   (Location.mkloc (Longident.Lident typ_name) loc, [])
                );
              ptyp_loc = loc;
              ptyp_attributes = [];
            }
          ];
          pcd_res = None;
          pcd_loc = loc;
          pcd_attributes = [];
        }
      )
      types
  in
  {
    ptype_name = Location.mkloc "tree" loc;
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind = Ptype_variant sum_members;
    ptype_private = Asttypes.Public;
    ptype_manifest = None;
    ptype_attributes = [];
    ptype_loc = loc;
  }

let mapper _ = {
  M.default_mapper with
  M.structure_item = (fun self item ->
      let desc =
        match item.pstr_desc with
        | Pstr_extension ((id, _), _)
          when id.Asttypes.txt = "build_tree_type" ->
          let types_list = collect_types
              "libs/ocplib-sempatch/lib/automaton/generator/parsetree.mli"
          in
          Pstr_type [(build_sum id.Asttypes.loc types_list)];
        | d -> d
      in
      M.default_mapper.M.structure_item self { item with pstr_desc = desc }
    );
}

let () = Ast_mapper.register "abc" mapper
