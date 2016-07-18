module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module S = String
open Asttypes
open Parsetree
open Std_utils

module Arg: Generic.ARG with type result = structure =
struct
  type result = structure
  type middle = value_binding

  let deriver_name = "match_builder"
  let raise_errorf = Common.raise_errorf

  let preamble =
    [%str
      let ignore_meta f m y = List.map (fun elt -> elt, m) (f y)

    ] @
    [%str
      let basic_state f = {
        A.transitions = [
          false,
          ignore_meta f
        ];
        final = false;
      }
    ]

  let generate_tuple input_list =
    match List.map (fun elt ->
        H.Exp.ident (L.mknoloc (LI.Lident elt)))
        input_list
    with
    | [] -> None
    | [elt] -> Some elt
    | lst -> Option.some @@ H.Exp.tuple lst

  let generate_patterns input_list =
    match List.map (fun _ -> H.Pat.any ()) input_list with
    | [] -> None
    | [elt] -> Some elt
    | lst -> Option.some @@ H.Pat.tuple lst

  let create_match name args pattern result =
    let here elt = L.mknoloc elt in
    let body =
      [%expr basic_state (function
          | [%p pattern] -> [[%e result]]
          | _ -> [A.Trash])
      ]
    and type_constraint = [%type: A.state]
    in
    H.Vb.mk
      (H.Pat.var (here name))
      (List.fold_left
         (fun expr arg_name ->
            let arg = H.Pat.constraint_
                (H.Pat.var (here arg_name))
                type_constraint
            in
            [%expr fun [%p arg] -> [%e expr]]
         )
         body
         (List.rev args)
      )

  let middle_of_record name fields =
    let here elt = L.mknoloc elt in
    let args = List.map (fun field ->field.pld_name.txt) fields in
    let pattern =
      let sub_pattern =
        H.Pat.record
          (List.map (fun name ->
               here @@ Longident.Lident (C.id name),
               H.Pat.any ()
             )
              args
          )
          Asttypes.Closed
      in
      H.Pat.construct
        (here @@ C.mk_exploded (Common.cstr name))
        (Some sub_pattern)
    and result =
      let sub_result =
        H.Exp.record
          (List.map (fun name ->
               here @@ C.mk_aut (C.id name),
               H.Exp.ident (here (Longident.Lident (C.id name)))
             )
              args
          )
          None
      in
      H.Exp.construct
        (here (C.mk_aut_cstr name))
        (Some sub_result)
    in
    [
      create_match
      name
      args
      pattern
      result
    ]

  let create_variant_match type_name variant =
    let variant_name = variant.pcd_name.txt in
    let here elt = L.mknoloc elt in
    let args = List.mapi (fun idx _ -> "state_" ^ (string_of_int idx))
        variant.pcd_args
    in
    let pattern =
      H.Pat.construct
        (here @@ C.mk_exploded (C.cstr type_name))
        (Some (
            H.Pat.construct
              (here @@ Longident.Lident (C.cstr variant_name))
              (generate_patterns variant.pcd_args)
          ))
    and result =
      H.Exp.construct
        (here (C.mk_aut_cstr type_name))
        (Some (
            H.Exp.construct
              (here (C.mk_aut_cstr variant_name))
              (generate_tuple args)
          ))
    in
    create_match
      (type_name ^ "_" ^ (S.uncapitalize variant_name))
      args
      pattern
      result

  let middle_of_variant name = List.map (create_variant_match name)

  let create_tuple_match name args =
    let here elt = L.mknoloc elt in
    let args = List.mapi (fun idx _ -> "state_" ^ (string_of_int idx))
        args
    in
    let pattern =
      H.Pat.construct
        (here (C.mk_exploded @@ Common.cstr name))
        (generate_patterns args)
    and result =
      H.Exp.construct
        (here (C.mk_aut_cstr name))
        (generate_tuple args)
    in
    create_match
      name
      args
      pattern
      result

  let middle_of_alias name recur type_declarations typ =
    match typ.ptyp_desc with
    | Ptyp_constr (_, []) ->
      let here elt = L.mknoloc elt in
      let args = ["state_0"] in
      let pattern =
        H.Pat.construct
          (here (C.mk_exploded @@ C.cstr name))
          (generate_patterns args)
      and result =
        H.Exp.construct
          (here (C.mk_aut_cstr name))
          (generate_tuple args)
      in
      create_match
        name
        args
        pattern
        result
      :: []
    | Ptyp_constr ({ txt = Longident.Lident id; _}, args) ->
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
            deriver_name id
      end
    | Ptyp_tuple args ->
      [create_tuple_match name args]
    | _ ->
      C.warn "Unable to create 'match' for %s\n" name;
      []

  let middle_of_abstract name =
          let pattern =
            H.Pat.construct
              (L.mknoloc (C.mk_exploded @@ C.cstr name))
              (Some [%pat? y])
          in
          let expr = [%expr
            fun x -> basic_state @@ function
              | [%p pattern] when x = y -> [A.Final]
              | _ -> [A.Trash]
          ]
          in
          H.Vb.mk
            (H.Pat.var (L.mknoloc name))
            expr

  let result_of_middle decls =
    let module_body = preamble @ [H.Str.value Nonrecursive decls] in
    [
      H.Str.module_
        (H.Mb.mk
           (Location.mknoloc "Match_")
           (H.Mod.structure module_body))
    ]
end

include Generic.Make(Arg)
