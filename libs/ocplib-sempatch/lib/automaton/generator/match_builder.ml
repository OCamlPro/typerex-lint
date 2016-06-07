module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module PpxD = Ppx_deriving
module S = String
open Asttypes
open Parsetree
open Std_utils

let deriver = "match_builder"

let raise_errorf = PpxD.raise_errorf

let preamble =
  [%str
   let ignore_meta f m y = List.map (fun elt -> elt, m) (f y)

   let basic_state f = {
       A.transitions = [
         false,
         ignore_meta f
       ];
       final = false;
     }

   let trash = {
       A.transitions = [];
       final = false;
     }
  ]

let generate_tuple loc input_list =
  match List.map (fun elt ->
                  H.Exp.ident ~loc (L.mkloc (LI.Lident elt) loc))
                 input_list
  with
  | [] -> None
  | [elt] -> Some elt
  | lst -> Option.some @@ H.Exp.tuple ~loc lst

let generate_patterns loc input_list =
  match List.map (fun _ -> H.Pat.any ~loc ()) input_list with
  | [] -> None
  | [elt] -> Some elt
  | lst -> Option.some @@ H.Pat.tuple ~loc lst

let mk_exploded str = Longident.Ldot (Longident.Lident "Element", str)
let mk_aut str = Longident.Ldot (Longident.Lident "A", str)
let mk_aut_cstr str = Longident.Ldot
    (Longident.Lident "A", S.capitalize str)

let create_match loc name args pattern result =
  let here elt = L.mkloc elt loc in
  let body =
    [%expr basic_state (function
                         | [%p pattern] -> [[%e result]]
                         | _ -> [A.Trash])
                                  [@@metaloc loc]
    ]
  and type_constraint = [%type: A.state]
  in
  H.Vb.mk
    ~loc
    (H.Pat.var ~loc (here name))
    (List.fold_left
       (fun expr arg_name ->
          let arg = H.Pat.constraint_
                      ~loc
                      (H.Pat.var ~loc (here arg_name))
                      type_constraint
          in
        [%expr fun [%p arg] -> [%e expr] [@@metaloc loc]]
       )
       body
       (List.rev args)
    )

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
  and result =
    let sub_result =
      H.Exp.record
        ~loc
        (List.map (fun name ->
             here @@ mk_aut (C.id name),
             H.Exp.ident ~loc (here (Longident.Lident (C.id name)))
           )
            args
        )
        None
    in
    H.Exp.construct
      ~loc
      (here (mk_aut_cstr name))
      (Some sub_result)
  in
  create_match
    loc
    name
    args
    pattern
    result

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
  and result =
    H.Exp.construct
      ~loc
      (here (mk_aut_cstr type_name))
      (Some (
           H.Exp.construct
             ~loc
             (here (mk_aut_cstr variant_name))
             (generate_tuple loc args)
         ))
  in
  create_match
    loc
    (type_name ^ "_" ^ (S.uncapitalize variant_name))
    args
    pattern
    result

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
  and result =
    H.Exp.construct
      ~loc
      (here (mk_aut_cstr name))
      (generate_tuple loc args)
  in
  create_match
    loc
    name
    args
    pattern
    result

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
    and result =
      H.Exp.construct
        ~loc
        (here (mk_aut_cstr name))
        (generate_tuple loc args)
    in
    create_match
      loc
      name
      args
      pattern
      result
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
            deriver id
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
              (Some [%pat? y])
          in
          let expr = [%expr
            fun x -> basic_state @@ function
              | [%p pattern] when x = y -> [A.Final]
              | _ -> [A.Trash]
          ]
          in
          [
            H.Vb.mk
              ~loc
              (H.Pat.var (L.mkloc name loc))
              expr
          ]
      end
    | _ -> assert false
  in
  let res =
    preamble @ [List.bind match_creator type_decls
                |> H.Str.value Nonrecursive]
  in
  [
    H.Str.module_
      (H.Mb.mk
         (Location.mknoloc "Match_")
         (H.Mod.structure res))
  ]
