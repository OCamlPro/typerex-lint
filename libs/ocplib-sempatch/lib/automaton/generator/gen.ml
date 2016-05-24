open Parsetree
module M = Ast_mapper
module H = Ast_helper

let list_type =
  let list_cases = H.Type.[
      constructor ("Nil" |> Location.mknoloc);
      constructor
        ("Cons" |> Location.mknoloc)
        ~args:H.Typ.[
            var "a";
            constr (Longident.Lident "list" |> Location.mknoloc) [var "a"]
          ];
    ]
  in
  H.Type.mk ~kind:(Ptype_variant list_cases) (Location.mknoloc "list")

let option_type =
  let option_cases = H.Typ.[
      constr (Longident.Lident "None" |> Location.mknoloc) [], Asttypes.Invariant;
      constr
        (Longident.Lident "Some" |> Location.mknoloc) [var "a"],
      Asttypes.Invariant;
    ]
  in
  H.Type.mk ~params:option_cases (Location.mknoloc "list")
let bool_type = H.Type.mk (Location.mknoloc "bool")
let int_type = H.Type.mk (Location.mknoloc "int")

let escape string =
  let rp s = Str.global_replace (Str.regexp_string s) in
  string
  |> rp " " "_"
  |> rp "(" "LPAR_"
  |> rp ")" "_RPAR"
  |> rp "*" "_AND_"
  |> rp "." "_DOT_"

let type_to_string typ =
  Pprintast.core_type Format.str_formatter typ;
  escape (Format.flush_str_formatter ())

let type_of_string str =
  Parser.parse_core_type Lexer.token (Lexing.from_string str)

let typ_collector met_types types_def = {
  M.default_mapper with
  M.typ = (fun self typ ->
      begin
        match typ.ptyp_desc with
        | Ptyp_constr _
        | Ptyp_tuple _
          ->
          met_types := (type_to_string typ, typ) :: !met_types
        | _ -> ()
      end;
      M.default_mapper.M.typ self typ
    );
  M.signature_item = (fun self item ->
      begin
        match item.psig_desc with
        | Psig_type types ->
          List.iter (fun typ ->
              Hashtbl.add
                types_def
                (Longident.Lident typ.ptype_name.Asttypes.txt)
                typ
            )
            types
        | _ -> ()
      end;
      M.default_mapper.M.signature_item self item
    );
}

let collect_types file =
  let types_list = ref []
  and types_def =
    let types_def = Hashtbl.create 10 in
    Hashtbl.add types_def (Longident.Lident "list") list_type;
    Hashtbl.add types_def (Longident.Lident "option") option_type;
    Hashtbl.add types_def (Longident.parse "Location.t") option_type;
    Hashtbl.add types_def (Longident.parse "Longident.t") option_type;
    types_def
  in
  let in_chan = open_in file in
  let parsed = Parser.interface Lexer.token (Lexing.from_channel in_chan) in
  let collector = typ_collector types_list types_def in
  ignore (collector.M.signature collector parsed);
  List.sort_uniq (fun (t1, _) (t2, _) ->
      String.compare t1 t2
    )
    !types_list, types_def

let build_sum loc types =
  let sum_members =
    List.map (fun (name, typ) ->
        {
          pcd_name = Location.mkloc (name |> String.capitalize) loc;
          pcd_args = [
            typ
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

let build_automaton_types loc tree_types tree_types_def =
  let state_core_type = {
    ptyp_loc = loc;
    ptyp_attributes = [];
    ptyp_desc = Ptyp_constr (Location.mkloc (Longident.Lident "state") loc, []);
  }
  in
  let types = List.map (fun (_, typ) ->
      let name = type_to_string typ in
      match typ.ptyp_desc with
      | Ptyp_tuple l ->
        H.Type.mk
          ~kind:Ptype_abstract
          ~manifest:{
            typ with
            ptyp_desc = Ptyp_tuple (List.map (fun _ -> state_core_type) l)
          }
          ("automaton_" ^ name |> Location.mknoloc)
      | Ptyp_constr ({Asttypes.txt = id; _}, _) ->
        let model_typ =
          begin
            try
              Hashtbl.find tree_types_def id
            with Not_found -> failwith name
          end
        in
        begin
          match model_typ.ptype_kind with
          | Ptype_variant v ->
            H.Type.mk ~kind:(Ptype_variant (List.map (fun constr_decl ->
                {
                  constr_decl with
                  pcd_name = (
                    "Automaton_" ^ name ^ "_" ^
                    (constr_decl.pcd_name.Asttypes.txt)
                  ) |> Location.mknoloc;
                  pcd_args =
                    List.map
                      (fun _ -> state_core_type)
                      constr_decl.pcd_args;
                }
              )
                v
              )
              )
              ("automaton_" ^ name |> Location.mknoloc)
          | Ptype_record r ->
            H.Type.mk ~kind:(Ptype_variant (List.map (fun label_decl ->
                {
                  pcd_name = (
                    "Automaton_" ^ name ^ "_" ^
                    (label_decl.pld_name.Asttypes.txt)
                  ) |> Location.mknoloc;
                  pcd_args = [state_core_type];
                  pcd_res = None;
                  pcd_loc = label_decl.pld_loc;
                  pcd_attributes = [];
                }
              )
                r
              )
              )
              ("automaton_" ^ name |> Location.mknoloc)
          | Ptype_abstract -> H.Type.mk ~manifest:state_core_type
              ("automaton_" ^ name |> Location.mknoloc)
          | _ -> assert false
        end
      | _ ->
        (* failwith name *)
        assert false
    )
      tree_types
  in
  let sum_of_all =
    H.Type.mk ~kind:(Ptype_variant (
        H.Type.constructor (Location.mknoloc "Trash") ::
        H.Type.constructor (Location.mknoloc "Final") ::
        List.map (fun typ ->
        {
          pcd_name = String.capitalize typ.ptype_name.Asttypes.txt
                     |> Location.mknoloc;
          pcd_args =
            [
              H.Typ.constr
                (Location.mknoloc (Longident.Lident typ.ptype_name.Asttypes.txt))
                []
            ];
          pcd_res = None;
          pcd_loc = loc;
          pcd_attributes = [];
        }
      )
        types
      )
      )
      (Location.mkloc "automaton_tree" loc)
  and transition_type = H.Type.mk
      ~manifest:(type_of_string
                   (
                     "bool * (Match.t -> Ast_element.t"
                     ^ "-> (automaton_tree * Match.t) list)"
                   )
                )
      (Location.mkloc "transition" loc)
  and state_type = H.Type.mk
      ~kind:(Ptype_record [
          H.Type.field
            ~mut:Asttypes.Mutable
            (Location.mknoloc "transitions")
            (type_of_string "transition list");
          H.Type.field
            ~mut:Asttypes.Mutable
            (Location.mknoloc "final")
            (H.Typ.constr (Location.mknoloc (Longident.Lident "bool")) []);
        ]
        )
      (Location.mkloc "state" loc)
  in
  sum_of_all::transition_type::state_type::types

let mapper _ =
  let types_list, types_def = collect_types
      "libs/ocplib-sempatch/lib/automaton/generator/parsetree.mli"
  in
  {
  M.default_mapper with
  M.structure_item = (fun self item ->
      let desc =
        match item.pstr_desc with
        | Pstr_extension ((id, _), _)
          when id.Asttypes.txt = "build_tree_type" ->
          Pstr_type [(build_sum id.Asttypes.loc types_list)];
        | Pstr_extension ((id, _), _)
          when id.Asttypes.txt = "build_automaton_types" ->
          Pstr_type (
            build_automaton_types id.Asttypes.loc types_list types_def
          );
        | d -> d
      in
      M.default_mapper.M.structure_item self { item with pstr_desc = desc }
    );
}

let () = Ast_mapper.register "abc" mapper
