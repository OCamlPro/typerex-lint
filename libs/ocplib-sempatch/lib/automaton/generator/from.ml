module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module S = String
open Asttypes
open Parsetree
open Std_utils

module Arg : Generic.ARG with type result = structure =
struct
  type result = structure
  type middle = string * expression

  let deriver_name = "from_builder"
  let raise_errorf = C.raise_errorf

  let overrides = C.get_val_decls [%str
      let pattern { ppat_desc; ppat_attributes; ppat_loc } =
        match ppat_desc with
        | Ppat_var { txt = id; _}
          when has_attr "__sempatch_metavar" ppat_attributes ->
          Match.metavar_pat id
        | _ ->
          Match.pattern (pattern_desc ppat_desc) (location__t ppat_loc)
            (attributes ppat_attributes)

      and expression { pexp_desc; pexp_loc; pexp_attributes } =
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

  let args_pattern case =
    let elts =
      List.mapi (fun idx _ ->
          H.Pat.var
            (L.mknoloc (nth_arg idx))
        )
        case.pcd_args
    in
    match elts with
    | [] -> None
    | [elt] -> Some elt
    | _ -> Some (H.Pat.tuple elts)

  let generate_froms typ =
    match Common.upprint typ with
    | None -> [%expr ()]
    | Some name ->
      (H.Exp.ident
         (L.mknoloc
            (LI.Lident (C.id name))
         ))

  let generate_sub_match typ var_name =
    H.Exp.apply
      (generate_froms typ)
      ["", H.Exp.ident (L.mknoloc (LI.Lident var_name))]

  let build_sub_from index typ =
    let arg_name = "sub_" ^ (string_of_int index) in
    "",
    H.Exp.apply
      (generate_froms typ)
      ["", H.Exp.ident (L.mknoloc (LI.Lident arg_name))]

  let apply_from name args =
    let matcher = H.Exp.ident (L.mknoloc (C.mk_match name)) in
    let result =
      match args with
      | [] -> matcher
      | _ ->
        H.Exp.apply
          matcher
          (List.mapi (build_sub_from) args)
    in [%expr ([%e result] :  A.state)]

  let middle_of_alias name self env typ =
    let template pattern expression =
      [name, [%expr fun [%p pattern] -> [%e expression]]]
    in
    match typ.ptyp_desc with
    | Ptyp_tuple args ->
      let names = List.mapi (fun idx typ -> nth_arg idx, typ) args in
      let pattern = H.Pat.tuple
          (List.map (fun (arg, _) ->
               H.Pat.var
                 (L.mknoloc arg)
             )
              names
          )
      and expr = H.Exp.apply
          (H.Exp.ident (Location.mknoloc
                          (C.mk_match name)))
          (List.map (fun (name, typ) ->
               "", H.Exp.apply
                   (generate_froms typ)
                   [("",(H.Exp.ident
                           (L.mknoloc (LI.Lident name))))]
             )
              names
          )
      in template pattern expr
    | Ptyp_constr ({ txt = id; _ }, []) ->
      let pattern = H.Pat.construct
          (Location.mknoloc
             (C.mk_exploded @@
              C.cstr (Longident.last id))
          )
          (Some (H.Pat.var (Location.mknoloc "y")))
      in
      [
        name,
        [%expr fun x -> Match.basic_state @@
        function
        | [%p pattern] when x = y -> [A.Final]
        | _ -> [A.Trash]]
      ]
    | Ptyp_constr ({ txt = Longident.Lident id; _ }, args) ->
      begin
        try
          let generic_type =
            List.find (fun typ -> typ.ptype_name.txt = id) env
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
          raise_errorf "%s : Not in the stdlib nor declared here : %s" deriver_name id
      end
    | _ ->
      C.warn "Unable to create 'from' for %s\n" name;
      template [%pat? _] [%expr ()]

  let middle_of_abstract name =
    name,
    H.Exp.ident
      (Location.mknoloc (C.mk_match @@
             C.id name))

  let middle_of_variant name cases =
    let exp =
      H.Exp.function_
        (List.map
           (fun case ->
              H.Exp.case
                (H.Pat.construct
                   (Location.mknoloc (LI.Lident (C.cstr case.pcd_name.txt)))
                   (args_pattern case)
                )
                (apply_from
                   (name ^ "_" ^ Common.id case.pcd_name.txt)
                   case.pcd_args
                )
           )
           cases
        )
    in [name, exp]

  let middle_of_record name fields =
    let exp =
      H.Exp.fun_
        ""
        None
        (H.Pat.record
           (List.map (fun field ->
                let name = field.pld_name.txt in
                (Location.mknoloc (LI.Lident name)),
                H.Pat.var (Location.mknoloc name)
              )
               fields)
           Closed
        )
        (
          H.Exp.apply
            (H.Exp.ident (L.mknoloc (C.mk_match name)))
            (List.map
               (fun field ->
                  "",
                  generate_sub_match
                    field.pld_type
                    field.pld_name.txt
               )
               fields
            )
        )
    in [name, exp]

  let result_of_middle middles =
    let overrided = apply_overrides @@ List.map
        (fun (name, e) -> H.Vb.mk (H.Pat.var (Location.mknoloc name)) e)
        middles
    in
    [
      H.Str.module_
        (H.Mb.mk
           (Location.mknoloc "From")
           (H.Mod.structure
              [
                overrided
                |> H.Str.value Recursive
              ]
           )
        )
    ]
end

include Generic.Make(Arg)
