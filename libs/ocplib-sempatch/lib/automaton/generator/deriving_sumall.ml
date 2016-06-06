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
                      (unit -> Ast_element.t
                       -> (t * unit)
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

let str_of_type type_decls =
  match type_decls with
  | [] -> []
  | hd::_ ->
    let loc =
      hd.ptype_loc
    in
    let instantiated_polys = Type_collector.collect type_decls in
    let poly_decls = (type_decls @ Common.stdlib) @ instantiated_polys in
    let mono_decls = Common.filter_decls poly_decls in
    let sum_typ, automaton_typ = sum_of_types loc mono_decls poly_decls in
    let automaton_typ = preprocess_automaton automaton_typ in
    let automaton_tree = List.map build_automaton_tree poly_decls in
    let constructors =
      [
        (* An "exploded" -- ie with more detailled types version of
        the parsetree *)
        "Exploded", [], mk_synonyms loc None mono_decls;

        (* Module conatining the sum type of all nodes in the ast *)
        "Ast_element", [], sum_typ ::
                        (mk_synonyms loc
                           (Some (Longident.Lident "Exploded")) mono_decls);

        (* The type of the automaton *)
        "A", [], transition_type :: state_type :: automaton_typ
                         :: automaton_tree;
      ]
    in
    let declarations =
          List.map
            (fun (name, preamble, sum_type) ->
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
            )
            constructors
    in
    declarations
    @ (Match_builder.str_of_type poly_decls mono_decls)
    @ (From.str_of_type poly_decls mono_decls)

let is_def_of name = List.exists (fun def -> def.ptype_name.txt = name)

let mapper = let open M in {
    default_mapper with
    structure = (fun self str ->
        List.bind (fun stri ->
            match stri.pstr_desc with
            | Pstr_extension ((id, _), _)
              when id.txt = "create_automaton" ->
              let in_file = open_in
                  ("/home/regnat/Documents/X/stage3A/typerex-lint/" ^
                   "libs/ocplib-sempatch/lib/automaton/generator/tree.ml")
              in
              let tree_struct =
                Parser.interface Lexer.token (Lexing.from_channel in_file)
              in
              List.bind (fun x ->
                  match x.psig_desc with
                  | Psig_type types when is_def_of "expression" types ->
                    str_of_type types
                  | _ -> []
                )
                tree_struct
            | _ ->
              [default_mapper.structure_item self stri]
          )
          str
      );
  }

let () = Ast_mapper.register "automaton" (fun _ -> mapper)
