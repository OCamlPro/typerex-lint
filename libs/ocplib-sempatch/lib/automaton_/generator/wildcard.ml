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
  type middle = case

  let raise_errorf = C.raise_errorf

  let deriver_name = "wildcard"

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

  let generate_tuple input_list =
    match dispatch input_list (fun _ result -> result)
    with
    | [] -> [None]
    | [[elt]] -> [Some elt]
    | lst -> List.map (fun elt -> Option.some @@ H.Exp.tuple elt) lst

  let generate_patterns input_list =
    match List.map (fun _ -> H.Pat.any ()) input_list with
    | [] -> None
    | [elt] -> Some elt
    | lst -> Option.some @@ H.Pat.tuple lst

  let create_match pattern results =
    H.Exp.case
      pattern
      (expr_list_to_list_expr results)

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
    and results =
      let sub_results =
        List.map (fun arg -> H.Exp.record arg None)
          (dispatch
             args
             (fun name result ->
                here @@ C.mk_aut (C.id name),
                result
             )
          )
      in
      List.map (fun sub -> H.Exp.construct
                   (here (C.mk_aut_cstr name))
                   (Some sub)
               )
        sub_results
    in
    [create_match pattern results]

  let middle_of_variant_field type_name variant =
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
    and results =
      let sub_results = generate_tuple args in
      List.map (fun sub ->
          H.Exp.construct
            (here (C.mk_aut_cstr type_name))
            (Some (
                H.Exp.construct
                  (here (C.mk_aut_cstr variant_name))
                  sub
              ))
        )
        sub_results
    in
    create_match pattern results

  let middle_of_variant name = List.map (middle_of_variant_field name)

  let create_tuple_match name args =
    let here elt = L.mknoloc elt in
    let args = List.mapi (fun idx _ -> "state_" ^ (string_of_int idx))
        args
    in
    let pattern =
      H.Pat.construct
        (here (C.mk_exploded @@ Common.cstr name))
        (generate_patterns args)
    and results =
      let sub_results = generate_tuple args in
      List.map (H.Exp.construct (here (C.mk_aut_cstr name))) sub_results
    in
    create_match
      pattern
      results

  let middle_of_alias name recur type_declarations typ =
    match typ.ptyp_desc with
    | Ptyp_constr (_, []) ->
      let here elt = L.mknoloc elt in
      let args = ["state_0"] in
      let pattern =
        H.Pat.construct
          (here (C.mk_exploded @@ C.cstr name))
          (generate_patterns args)
      and results =
        let sub_results = generate_tuple args in
        List.map (H.Exp.construct (here (C.mk_aut_cstr name))) sub_results
      in
      create_match
        pattern
        results
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
        (L.mknoloc (C.mk_exploded @@
                    C.cstr name))
        (Some [%pat? _])
    in
    let expr = [%expr [A.Trash]]
    in
      H.Exp.case
        pattern
        expr

  let result_of_middle middles =
      let transition_fun =
        H.Exp.function_ middles
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
end

include Generic.Make(Arg)
