module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module PpxD = Ppx_deriving
module S = String
open Asttypes
open Parsetree
open Std_utils

let deriver = "from_builder"

let raise_errorf = PpxD.raise_errorf

let overrides = C.get_val_decls [%str
    let expression { pexp_desc; pexp_loc; pexp_attributes } =
      match pexp_desc with
      | Pexp_extension ({ Asttypes.txt = "__sempatch_report"; _},
                        PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
        make_report @@ expression e
      | Pexp_extension ({ Asttypes.txt = "__sempatch_inside"; _},
                        PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
        add_transitions_from (Match.wildcard ()) (expression e)
      | Pexp_ident ( { txt = Longident.Lident id; _ })
        when has_attr "__sempatch_metavar" pexp_attributes ->
        Match.metavar_expr id
      | _ ->
        Match.expression (expression_desc pexp_desc) (location__t pexp_loc)
          (attributes pexp_attributes)
  ]

let same_def vb1 vb2 =
  match vb1.pvb_pat.ppat_desc, vb2.pvb_pat.ppat_desc with
  | Ppat_var v1, Ppat_var v2 -> v1.txt = v2.txt
  | _ -> false

let apply_overrides = List.map (
    fun type_ ->
      List.find_opt (same_def type_) overrides
      |> Option.value type_
  )

let nth_arg n = Printf.sprintf "sub_%i" n

let args_pattern loc case =
  let elts =
    List.mapi (fun idx _ ->
               H.Pat.var
                 ~loc
                 (L.mkloc (nth_arg idx) loc)
              )
              case.pcd_args
  in
  match elts with
  | [] -> None
  | [elt] -> Some elt
  | _ -> Some (H.Pat.tuple ~loc elts)

let generate_froms loc typ =
  match Common.upprint typ with
  | None -> [%expr ()]
  | Some name ->
     (H.Exp.ident
        ~loc
        (L.mkloc
           (LI.Lident (C.id name))
           loc))

let generate_sub_match loc typ var_name =
  H.Exp.apply
    ~loc
    (generate_froms loc typ)
    ["", H.Exp.ident ~loc (L.mkloc (LI.Lident var_name) loc)]

let build_sub_from loc index typ =
  let arg_name = "sub_" ^ (string_of_int index) in
  "",
  H.Exp.apply
    ~loc
    (generate_froms loc typ)
    ["", H.Exp.ident ~loc (L.mkloc (LI.Lident arg_name) loc)]

let apply_from loc name args =
  let matcher = H.Exp.ident (L.mkloc (LI.Ldot (LI.Lident "Match", name)) loc) in
  let result =
    match args with
    | [] -> matcher
    | _ ->
      H.Exp.apply
        ~loc
        matcher
        (List.mapi (build_sub_from loc) args)
  in [%expr ([%e result] :  A.state)]

let str_of_core_type self type_declarations loc name typ =
  let template pattern expression =
    [%expr fun [%p pattern] -> [%e expression]]
  in
  match typ.ptyp_desc with
  | Ptyp_tuple args ->
    let names = List.mapi (fun idx typ -> nth_arg idx, typ) args in
    let pattern = H.Pat.tuple
      ~loc
      (List.map (fun (arg, _) ->
           H.Pat.var
             ~loc
             (L.mkloc arg loc)
         )
          names
      )
    and expr = H.Exp.apply
        ~loc
        (H.Exp.ident ~loc (Location.mkloc
                             (LI.Ldot (LI.Lident "Match", name)) loc))
        (List.map (fun (name, typ) ->
             "", H.Exp.apply
               ~loc
               (generate_froms loc typ)
               [("",(H.Exp.ident
                  ~loc
                  (L.mkloc (LI.Lident name) loc)))]
           )
            names
        )
    in template pattern expr
  | Ptyp_constr ({ txt = id; _ }, []) ->
        let pattern = H.Pat.construct
            ~loc
            (Location.mkloc
               (LI.Ldot (LI.Lident "Element",
                         C.cstr (Longident.last id)))
               loc)
            (Some (H.Pat.var ~loc (Location.mkloc "y" loc)))
        in
        [%expr fun x -> Match.basic_state @@
                 function
                 | [%p pattern] when x = y -> [A.Final]
                 | _ -> [A.Trash]]
  | Ptyp_constr ({ txt = Longident.Lident id; _ }, args) ->
    begin
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
                  | _ ->
                    C.warn "Unable to create 'from' for %s\n" name;
                    []
               )
               generic_type.ptype_params
            )
            args
        in
        let real_type =
          C.instantiate_type_decl instanciations generic_type
        in
        self { real_type with ptype_name = Location.mknoloc name }
      with
        Not_found ->
        raise_errorf "%s : Not in the stdlib nor declared here : %s" deriver id
    end
  | _ ->
    C.warn "Unable to create 'from' for %s\n" name;
    template [%pat? _] [%expr ()]

let rec from_builder_of_type loc type_decls name type_decl =
  let here elt = Location.mkloc elt loc in
    match type_decl.ptype_kind with
    | Ptype_variant cases ->
       H.Exp.function_
         ~loc
         (List.map
            (fun case ->
             H.Exp.case
               (H.Pat.construct
                  ~loc
                  (here (LI.Lident (C.cstr case.pcd_name.txt)))
                  (args_pattern loc case)
               )
               (apply_from loc
                           (name ^ "_" ^ Common.id case.pcd_name.txt)
                           case.pcd_args
               )
            )
            cases
         )
    | Ptype_record fields ->
       H.Exp.fun_
         ""
         None
         (H.Pat.record
            ~loc
            (List.map (fun field ->
                       let name = field.pld_name.txt in
                       (here (LI.Lident name)),
                       H.Pat.var ~loc (here name)
                      )
                      fields)
            Closed
         )
         (
           H.Exp.apply
             ~loc
             (H.Exp.ident (L.mkloc (LI.Ldot (LI.Lident "Match", (name))) loc))
             (List.map
                (fun field ->
                 "",
                 generate_sub_match
                   loc
                   field.pld_type
                   field.pld_name.txt
                )
                fields
             )
         )
    | Ptype_abstract ->
       begin
         match type_decl.ptype_manifest with
         | None ->
           H.Exp.ident
             ~loc
             (here (LI.Ldot (LI.Lident "Match",
                C.id type_decl.ptype_name.txt)))
         | Some t ->
            str_of_core_type
              (from_builder_of_type loc type_decls type_decl.ptype_name.txt)
              type_decls loc type_decl.ptype_name.txt t
       end
    | Ptype_open
      ->
       raise_errorf
         ~loc:type_decl.ptype_loc
         "%s can't handle open types, sorry"
         deriver

let str_of_type all_decls type_decls =
  let from_creator type_decl =
    if type_decl.ptype_params <> [] then [] else
    let loc = type_decl.ptype_loc in
    let here elt = L.mkloc elt loc in
    let name = type_decl.ptype_name.txt in
    let def = from_builder_of_type loc all_decls name type_decl
    in
    [H.Vb.mk (H.Pat.var (here name)) def]
    |> apply_overrides
  in
  [
    H.Str.module_
      (H.Mb.mk
         (Location.mknoloc "From")
         (H.Mod.structure
            [
              List.bind from_creator type_decls
              |> H.Str.value Recursive
            ]
         )
      )
  ]
