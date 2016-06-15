module H = Ast_helper
module L = Location
module LI = Longident
module M = Ast_mapper
module PpxD = Ppx_deriving
module S = String
open Asttypes
open Parsetree
open Std_utils

let transition_type = H.Type.mk
    ~manifest:[%type: Exploded.bool *
                      (Match.t -> Ast_element.t
                       -> (t * Match.t)
                         Ppx_deriving_runtime.list)]
    (L.mknoloc "transition")
and state_type = H.Type.mk
    ~kind:(Ptype_record [
        H.Type.field
          ~mut:Asttypes.Mutable
          (L.mknoloc "transitions")
          [%type: transition Ppx_deriving_runtime.list];
        H.Type.field
          ~mut:Asttypes.Mutable
          (L.mknoloc "final")
          [%type: Exploded.bool];
      ]
      )
    (L.mknoloc "state")

let trash_variant =
  H.Type.constructor (Location.mknoloc "Trash")
and final_variant =
  H.Type.constructor (Location.mknoloc "Final")

let variant_of_type_decl loc type_decl =
  let constr_name, type_lid =
        Common.cstr type_decl.ptype_name.txt,
      LI.Lident (Common.id type_decl.ptype_name.txt)
  in
  let args =
    match type_decl.ptype_manifest with
    | Some typ -> [typ]
    | None ->
      [H.Typ.constr
         ~loc
         (L.mkloc type_lid loc)
         (type_decl.ptype_params |> List.map fst)]
  in
  H.Type.constructor
    ~loc
    ~args
    (L.mkloc constr_name loc)

let mk_synonyms loc prefix =
  let has_prefix = Option.is_some prefix in
  let attrs =
    if has_prefix then [] else
      [Location.mkloc "nonrec" loc, PStr []]
  and mkname name =
    match prefix with
    | None -> Longident.Lident name
    | Some ident -> Longident.Ldot (ident, name)
  in
  let mk_synonym decl =
    let name = decl.ptype_name.txt in
    match decl.ptype_manifest with
    | None ->
      H.Type.mk
        ~loc
        ~attrs
        ~params:decl.ptype_params
        ~kind:decl.ptype_kind
        ~manifest:(H.Typ.constr ~loc
                     (Location.mkloc
                        (mkname name) loc) (List.map fst decl.ptype_params))
        (Location.mkloc name loc)
    | Some _ -> decl
  in
  List.map mk_synonym

let build_automaton_tree model =
  let module P = Automaton_processor in
  model
  |> (P.remove_arguments.M.type_declaration P.remove_arguments)
  |> (P.convert_to_states.M.type_declaration P.convert_to_states)

let preprocess_automaton decl =
  let module P = Automaton_processor in
  let convert_inside_tuples =
    P.apply_to_tuples P.convert_to_states
  in
  decl
  |> (P.remove_arguments.M.type_declaration P.remove_arguments)
  |> (convert_inside_tuples.M.type_declaration convert_inside_tuples)

let sum_of_types loc types all_types =
  let mono_sum = List.map (variant_of_type_decl loc) types in
  let poly_sum = List.map (variant_of_type_decl loc) all_types in
  H.Type.mk
    ~loc
    ~kind:(Ptype_variant(mono_sum))
    (Location.mkloc "t" loc),
  H.Type.mk
    ~loc
    ~kind:(Ptype_variant(trash_variant :: final_variant :: poly_sum))
    (Location.mkloc "t" loc)

let str_of_type type_decls cmd =
  match type_decls with
  | [] -> []
  | hd::_ ->
    let loc =
      hd.ptype_loc
    in
    let instantiated_polys = Type_collector.collect type_decls in
    let poly_decls = (type_decls @ Common.stdlib) @ instantiated_polys in
    let mono_decls = Common.filter_decls poly_decls in
    let sum_typ, automaton_typ = sum_of_types loc mono_decls mono_decls in
    let automaton_typ = preprocess_automaton automaton_typ in
    let automaton_tree = List.map build_automaton_tree poly_decls in
    let mk_type_module =
      fun (name, preamble, sum_type) ->
        H.Str.module_
          ~loc
          (H.Mb.mk
             ~loc
             (L.mkloc name loc)
             (H.Mod.structure
                ~loc
                (preamble @
                 [H.Str.type_
                    ~loc
                    sum_type
                 ]
                )
             )
          )
    in
    match cmd with
    | `Ast_types ->
      let poly_decls_no_list =
        List.filter
          (fun decl -> not (decl.ptype_name.txt = "list"))
          poly_decls
      in
      let constructors =
        [
          (* An "exploded" -- ie with more detailled types version of
             the parsetree *)
          "Exploded", [], mk_synonyms loc None poly_decls_no_list;

          (* Module conatining the sum type of all nodes in the ast *)
          "Element", [], sum_typ ::
                             (mk_synonyms loc
                                (Some (Longident.Lident "Exploded"))
                                poly_decls_no_list);
        ]
      in
      let declarations =
        List.map
          mk_type_module
          constructors
      in
      declarations
    | `Automaton_types ->
      [mk_type_module (
          "A", [], transition_type :: state_type :: automaton_typ
                   :: automaton_tree;
        )]
    | `Match ->
      (Match_builder.str_of_type poly_decls mono_decls)
    | `Wildcard ->
      (Wildcard.str_of_type poly_decls mono_decls)
    | `From ->
      (From.str_of_type poly_decls mono_decls)
    | `Eval ->
      (Eval_builder.combine_all poly_decls mono_decls)

let is_def_of name = List.exists (fun def -> def.ptype_name.txt = name)

let mapper = let open M in
  let perform file cmd =
    let file = Option.value 
        ("libs/ocplib-sempatch/lib/automaton/generator/tree.ml")
        file
    in
    let in_file = open_in file
    in
    let tree_struct =
      Parser.interface Lexer.token (Lexing.from_channel in_file)
    in
    List.bind (fun x ->
        match x.psig_desc with
        | Psig_type types when is_def_of "expression" types ->
          str_of_type types cmd
        | _ -> []
      )
      tree_struct
  in
  {
    default_mapper with
    structure = (fun self str ->
        List.bind (fun stri ->
            match stri.pstr_desc with
            | Pstr_extension
                ((id, arg), _) ->
              let name = match arg with
                | PStr [%str
                    [%e? {
                         pexp_desc = Pexp_constant
                             (Asttypes.Const_string (name, None)); _
                       }
                    ]
                  ] -> Some name
                | _ -> None
              in
              let action =
                match id.txt with
                | "create_ast_element" -> Some `Ast_types
                | "create_automaton" -> Some `Automaton_types
                | "create_match" -> Some `Match
                | "create_from" -> Some `From
                | "create_eval" -> Some `Eval
                | "create_wildcard" -> Some `Wildcard
                | _ -> None
              in
              begin
                match action with
                | Some a -> perform name a
                | None -> [default_mapper.structure_item self stri]
              end
            | _ ->
              [default_mapper.structure_item self stri]
          )
          str
      );
  }

let () = Ast_mapper.register "automaton" (fun _ -> mapper)
