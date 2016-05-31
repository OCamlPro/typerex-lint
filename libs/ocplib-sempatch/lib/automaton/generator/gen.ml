open Parsetree
module M = Ast_mapper
module H = Ast_helper
module C = Common

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
  H.Type.mk ~params:[[%type: 'a], Asttypes.Invariant]
            ~kind:(Ptype_variant list_cases) (Location.mknoloc "list")

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

let typ_collector met_types types_def = {
  M.default_mapper with
  M.typ = (fun self typ ->
      begin
        match typ.ptyp_desc with
        | Ptyp_constr _
        | Ptyp_tuple _
          ->
          met_types := (C.type_to_string typ, typ) :: !met_types
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
          pcd_name = Location.mkloc (name |> C.cstr) loc;
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
let mapper args =
  let ast_path =
    match args with
    | path::_ -> path
    | _ -> "libs/ocplib-sempatch/lib/automaton/generator/tree.ml"
  in
  List.iter print_endline args;
  let types_list, types_def = collect_types ast_path
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
              Automaton_types.build id.Asttypes.loc types_list types_def
            );
          | Pstr_extension ((id, _), _)
            when id.Asttypes.txt = "build_automaton_matchers" ->
            Matches_generator.generate
              id.Asttypes.loc
              (build_sum id.Asttypes.loc types_list)
              types_def
          | Pstr_extension ((id, _), _)
            when id.Asttypes.txt = "build_automaton_froms" ->
            From_generator.generate
              id.Asttypes.loc
              types_list
              types_def
          | d -> d
        in
        M.default_mapper.M.structure_item self { item with pstr_desc = desc }
      );
  }

let () = Ast_mapper.register "abc" mapper
