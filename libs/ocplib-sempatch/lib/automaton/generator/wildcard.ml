module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module PpxD = Ppx_deriving
module S = String
open Asttypes
open Parsetree
open Std_utils

let raise_errorf =
  Printf.ksprintf failwith

let plugin_name = "wildcard"

let expr_list_to_list_expr exprs =
  match exprs with
  | [] -> [%expr [[Automaton.final (), env]]]
  | _ -> List.fold_left (
      fun accu expr ->
        [%expr [%e expr] :: [%e accu]]
    )
      [%expr []]
      exprs

let dispatch model constructor =
  List.mapi (
    fun idx _ ->
      List.mapi (
        fun jdx elt ->
          let result =
            if idx = jdx then [%expr state] else [%expr final ()]
          in
          constructor elt result
      )
        model
  )
    model

let generate_tuple loc input_list =
  match dispatch input_list (fun _ result -> result)
  with
  | [] -> [None]
  | [[elt]] -> [Some elt]
  | lst -> List.map (fun elt -> Option.some @@ H.Exp.tuple ~loc elt) lst

let generate_patterns loc input_list =
  match List.map (fun _ -> H.Pat.any ~loc ()) input_list with
  | [] -> None
  | [elt] -> Some elt
  | lst -> Option.some @@ H.Pat.tuple ~loc lst

let mk_exploded str = Longident.Ldot (Longident.Lident "Element", str)
let mk_aut str = Longident.Ldot (Longident.Lident "A", str)
let mk_aut_cstr str = Longident.Ldot
    (Longident.Lident "A", S.capitalize str)

let create_match pattern results =
  H.Exp.case
    pattern
    (expr_list_to_list_expr results)

let create_record_match loc name fields =
  let here elt = L.mkloc elt loc in
  let args = List.map (fun field ->field.pld_name.txt) fields in
  let pattern =
    let sub_pattern =
      H.Pat.record
        ~loc
        (List.map (fun name ->
             here @@ Longident.Lident (C.id name),
             H.Pat.any ~loc ()
           )
            args
        )
        Asttypes.Closed
    in
    H.Pat.construct
         ~loc
         (here @@ mk_exploded (Common.cstr name))
         (Some sub_pattern)
  and results =
    let sub_results =
      List.map (fun arg -> H.Exp.record ~loc arg None)
        (dispatch
           args
           (fun name result ->
              here @@ mk_aut (C.id name),
              result
           )
        )
    in
    List.map (fun sub -> H.Exp.construct
                 ~loc
                 (here (mk_aut_cstr name))
                 (Some sub)
             )
      sub_results
  in
  create_match pattern results

let create_variant_match loc type_name variant =
  let variant_name = variant.pcd_name.txt in
  let here elt = L.mkloc elt loc in
  let args = List.mapi (fun idx _ -> "state_" ^ (string_of_int idx))
                       variant.pcd_args
  in
  let pattern =
    H.Pat.construct
      ~loc
      (here @@ mk_exploded (C.cstr type_name))
      (Some (
           H.Pat.construct
             ~loc
             (here @@ Longident.Lident (C.cstr variant_name))
             (generate_patterns loc variant.pcd_args)
         ))
  and results =
    let sub_results = generate_tuple loc args in
    List.map (fun sub ->
        H.Exp.construct
          ~loc
          (here (mk_aut_cstr type_name))
          (Some (
              H.Exp.construct
                ~loc
                (here (mk_aut_cstr variant_name))
                sub
            ))
      )
      sub_results
  in
  create_match
    pattern
    results

let create_tuple_match loc name args =
  let here elt = L.mkloc elt loc in
  let args = List.mapi (fun idx _ -> "state_" ^ (string_of_int idx))
      args
  in
  let pattern =
    H.Pat.construct
      ~loc
      (here (mk_exploded @@ Common.cstr name))
      (generate_patterns loc args)
  and results =
    let sub_results = generate_tuple loc args in
    List.map (H.Exp.construct ~loc (here (mk_aut_cstr name))) sub_results
  in
  create_match
    pattern
    results

let create_core_typ_match recur type_declarations loc name typ =
  match typ.ptyp_desc with
  | Ptyp_constr (_, []) ->
    let here elt = L.mkloc elt loc in
    let args = ["state_0"] in
    let pattern =
      H.Pat.construct
        ~loc
        (here (mk_exploded @@ C.cstr name))
        (generate_patterns loc args)
    and results =
      let sub_results = generate_tuple loc args in
      List.map (H.Exp.construct ~loc (here (mk_aut_cstr name))) sub_results
    in
    create_match
      pattern
      results
    :: []
  | Ptyp_constr ({ txt = Longident.Lident id; _}, args) ->
    begin
      (* match C.upprint typ with *)
      (* | Some alias when alias <> name -> *)
      (*   let here elt = L.mkloc elt loc in *)
      (*   let args = ["state_0"] in *)
      (*   let pattern = *)
      (*     H.Pat.construct *)
      (*       ~loc *)
      (*       (here (mk_exploded @@ C.cstr name)) *)
      (*       (generate_patterns loc args) *)
      (*   and result = *)
      (*     H.Exp.construct *)
      (*       ~loc *)
      (*       (here (mk_aut_cstr name)) *)
      (*       (generate_tuple loc args) *)
      (*   in *)
      (*   create_match *)
      (*     loc *)
      (*     name *)
      (*     args *)
      (*     pattern *)
      (*     result *)
      (*   :: [] *)
      (*   (* let expr = *) *)
      (*   (*   H.Exp.ident ~loc *) *)
      (*   (*     (L.mkloc (LI.Lident (C.id alias)) loc) *) *)
      (*   (* in *) *)
      (*   (* [H.Vb.mk *) *)
      (*   (*    ~loc *) *)
      (*   (*    (H.Pat.var ~loc (L.mkloc name loc)) *) *)
      (*   (*    [%expr fun x -> [%e expr] x] *) *)
      (*   (* ] *) *)
      (* | _ -> *)
        try
          let generic_type =
            List.find (fun typ -> typ.ptype_name.txt = id) type_declarations
          in
          let instanciations =
            List.combine
              (List.bind
                 (fun (typ, _) ->
                    match typ.ptyp_desc with
                    | Ptyp_var v -> [v]
                    | _ -> []
                 )
                 generic_type.ptype_params
              )
              args
          in
          let real_type =
            C.instantiate_type_decl instanciations generic_type
          in
          recur { real_type with ptype_name = Location.mknoloc name }
        with
          Not_found ->
          raise_errorf "%s : Not in the stdlib nor declared here : %s"
            plugin_name id
    end
  | Ptyp_tuple args ->
    [create_tuple_match loc name args]
  | _ ->
    C.warn "Unable to create 'match' for %s\n" name;
    []

let str_of_type all_types type_decls =
  let rec match_creator type_decl =
    let loc = type_decl.ptype_loc in
    let name = type_decl.ptype_name.txt in
    match type_decl.ptype_kind with
    | Ptype_variant cases ->
       List.map
         (create_variant_match loc name)
         cases
    | Ptype_record fields ->
      [create_record_match loc name fields]
    | Ptype_abstract ->
      begin
        match type_decl.ptype_manifest with
        | Some typ ->
          create_core_typ_match match_creator all_types loc name typ
        | None ->
          let pattern =
            H.Pat.construct
              ~loc
              (L.mkloc (LI.Ldot (
                  LI.Lident "Element",
                  C.cstr type_decl.ptype_name.txt)) loc)
              (Some [%pat? _])
          in
          let expr = [%expr [A.Trash]]
          in
          [
            H.Exp.case
              pattern
              expr
          ]
      end
    | _ -> assert false
  in
  let transition_fun =
    H.Exp.function_ (List.bind match_creator type_decls)
  in
  [%str
    let wildcard () =
      let state = A.{
        final = false;
        transitions = [];
      }
      in
      state.A.transitions <-
        [
          false,
          ignore_meta @@ [%e transition_fun]
        ];
      state
  ]

