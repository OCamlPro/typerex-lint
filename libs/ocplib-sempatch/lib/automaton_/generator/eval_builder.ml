module C = Common
module H = Ast_helper
module L = Location
module LI = Longident
module S = String
open Asttypes
open Parsetree
open Std_utils

(*
   The file to generate will be structured like this :

   module A = Automaton.A
   module T = Ast_element.Element

   let rec dispatch env states element =
     let results =
       match states, element with
       | A.Foo states, T.Foo element ->
           dispatch_bar env states element;
       | ... -> ...
     in match results with
     | [] -> []
     | hd::tl -> List.fold_left (List.product_bind both) hd tl

   and dispatch_bar env states element =
   ...
*)

let static_bindings =
  List.bind (
    fun stri ->
      match stri.pstr_desc with
      | Pstr_value (_, decls) -> decls
      | _ -> []
  )
    [%str

      let rec apply' = fun env state node ->
        if state.A.final then
          [state, env]
        else
          let env = match node with
            | T.Expression e ->
              {env with Match.current_location = e.Parsetree.pexp_loc}
            | _ -> env
          in
          let new_states = List.bind
              (fun (update_loc, trans) ->
                 let new_loc =
                   if update_loc then
                     Some (Match.get_current_location env)
                   else
                     Match.get_location env
                 in
                 let env = Match.set_location new_loc env in
                 trans env node
              )
              state.A.transitions
          in
          dispatch' new_states node
      and dispatch' = fun state_bundles expr ->
        List.bind (fun (state_bun, env) -> dispatch env state_bun expr)
          state_bundles
    ]

let mk_prefixed prefix id = LI.Ldot (LI.Lident prefix, id)

let mk_a = C.mk_aut
let mk_t id =
  match id with
  | "Nil" -> LI.Lident "[]"
  | "Cons" -> LI.Lident "::"
  | _ -> mk_prefixed "T" id

let mk_case loc decl =
  let here elt = L.mkloc elt loc in
  let name = decl.ptype_name.txt in
  let partial_pattern cstr var_name =
    let sub_pattern =
      match var_name with
      | Some name ->
        H.Pat.var ~loc (here name)
      | None -> H.Pat.any ~loc ()
    in
    H.Pat.construct
      ~loc
      (here @@ cstr (C.cstr name))
      (Some sub_pattern)
  in
  let pattern =
    H.Pat.tuple
      ~loc
      [
        partial_pattern mk_a (Some "states");
        partial_pattern mk_t (Some "element");
      ]
  and expression =
    let aux_function =
      H.Exp.ident
        ~loc
        (here @@ LI.Lident (C.id name))
    in
    [%expr
      [%e aux_function] env states element
    ]
  in
  [
    H.Exp.case
      pattern
      expression;
  ]

let global_dispatcher loc decls =
  let trash_case =
    H.Exp.case
      [%pat? A.Trash, _]
      [%expr []]
  and final_case =
    H.Exp.case
      [%pat? A.Final, _]
      [%expr [lazy [Automaton.final (), env]]]
  in
  (* The list of cases in the big "match" part *)
  let cases = trash_case :: final_case :: List.bind (mk_case loc) decls
  in
  let cases =
    cases @
    [H.Exp.case
       (H.Pat.any ~loc ())
       [%expr [lazy [Automaton.trash (), env]]]]
  in
  let inside_match =
    H.Exp.match_
      ~loc
      [%expr states, element]
      cases
  in
  let value =
    [%expr
      fun env (states : A.t) (element : T.t) ->
        let results = [%e inside_match] in
        match results with
        | [] -> []
        | hd::tl -> List.fold_left
                      (semilazy_product_bind both)
                      (Lazy.force hd) tl
    ]
  in
  H.Vb.mk
    ~loc
    [%pat? dispatch]
    value

let expr_list_to_list_expr exprs =
  match exprs with
  | [] -> [%expr [lazy [Automaton.final (), env]]]
  | _ -> List.fold_left (
      fun accu expr ->
        [%expr (lazy [%e expr]) :: [%e accu]]
    )
      [%expr []]
      exprs

let apply' states element = [%expr apply' env [%e states] [%e element]]

let name_args =
  List.mapi (fun idx typ ->
      "sub_states_" ^ string_of_int idx,
      "sub_element_" ^ string_of_int idx,
      typ
    )

let generate_match_on_variant loc cases =
  let here elt = L.mkloc elt loc in
  let generate_match case =
    let name = case.pcd_name.txt in
    let args = name_args case.pcd_args
    in
    let mk_sub_pattern constr getter =
      H.Pat.construct
        ~loc
        (here @@ constr name)
        (match args with
         | [] -> None
         | [arg] -> (Some (H.Pat.var ~loc (here @@ getter arg)))
         | _ ->
           Some (H.Pat.tuple
                   ~loc
                   (List.map (fun a -> H.Pat.var ~loc (here @@ getter a)) args))
        )
    in
    let pattern =
      H.Pat.tuple
        ~loc
        [
          mk_sub_pattern mk_a (fun (x,_,_) -> x);
          mk_sub_pattern mk_t (fun (_,x,_) -> x);
        ]
    and expressions =
      List.map (
        fun (states, element, typ) ->
          let states = H.Exp.ident ~loc (here @@ LI.Lident states)
          and element =
            match C.upprint typ with
            | None -> assert false
            | Some repr ->
              H.Exp.construct
                ~loc
                (here @@ mk_t (C.cstr repr))
                (Some (H.Exp.ident ~loc (here @@ LI.Lident element)))
          in
          apply' states element
      )
        args
    in let expression = expr_list_to_list_expr expressions
    in
    [
      H.Exp.case
        pattern
        expression;
    ]
  in
  List.bind generate_match cases

let generate_dispatch_on_record loc fields =
  let here elt = L.mkloc elt loc in
  let generate_from_field { pld_name = name; pld_type = typ; _} =
    match C.upprint typ with
    | None -> assert false
    | Some repr ->
      let states = H.Exp.field ~loc [%expr states] (here @@ mk_a name.txt)
      and element =
        H.Exp.construct
          ~loc
          (here @@ mk_t (C.cstr repr))
          (Some (H.Exp.field ~loc [%expr element] (here @@ mk_t name.txt)))
      in
      apply' states element
  in
  List.map generate_from_field fields
  |> expr_list_to_list_expr

(* Those has to be recursive because [generate_dispatch_on_abstract_type] can
   call back [generate_dispatch_body] when the type is just a synonym of
   another one *)
let rec generate_dispatch_on_abstract_type loc decls_in_env typ =
  let here elt = L.mkloc elt loc in
  match typ.ptyp_desc with
  | Ptyp_tuple types ->
    let args = name_args types in
    let mk_sub_pattern getter =
      H.Pat.tuple
        ~loc
        (List.map (fun a -> H.Pat.var ~loc (here @@ getter a)) args)
    in
    let pattern =
      H.Pat.tuple
        ~loc
        [
          mk_sub_pattern (fun (x,_,_) -> x);
          mk_sub_pattern (fun (_,x,_) -> x);
        ]
    and expressions =
      List.map (
        fun (states, element, typ) ->
          let states = H.Exp.ident ~loc (here @@ LI.Lident states)
          and element =
            match C.upprint typ with
            | None -> assert false
            | Some repr ->
              H.Exp.construct
                ~loc
                (here @@ mk_t (C.cstr repr))
                (Some (H.Exp.ident ~loc (here @@ LI.Lident element)))
          in
          apply' states element
      )
        args
    in let expression = expr_list_to_list_expr expressions
    in
    [%expr let [%p pattern] = states, element in [%e expression]]
  | Ptyp_constr (_, []) ->
    C.warn "Unimplemented : constructor without arguments\n";
    [%expr assert false]
  | Ptyp_constr ({ txt = Longident.Lident id; _ }, args) ->
    begin
      try
        let generic_type =
          List.find (fun typ -> typ.ptype_name.txt = id) decls_in_env
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
        generate_dispatch_body loc decls_in_env real_type
      with
        Not_found ->
        Printf.kprintf failwith "%s : Not in the stdlib nor declared here : %s"
          "eval" id
    end
  | _ ->
    C.warn "Unimplemented type\n";
    [%expr assert false]

and generate_dispatch_body loc decls_in_env decl =
  match decl.ptype_kind with
  | Ptype_variant cases ->
    let cases = generate_match_on_variant loc cases in
    let cases =
      cases @
      [H.Exp.case
         (H.Pat.any ~loc ())
         [%expr [lazy [Automaton.trash (), env]]]]
    in
    H.Exp.match_
      ~loc
      [%expr states, element]
      cases
  | Ptype_record labels ->
    generate_dispatch_on_record loc labels
  | Ptype_abstract ->
    begin match decl.ptype_manifest with
      | Some typ ->
        generate_dispatch_on_abstract_type loc decls_in_env typ
      | None ->
        (* An abstract type should always return Final or Trash *)
        [%expr ignore env; ignore states; ignore element; assert false]
    end
  | Ptype_open ->
    C.warn "Unable to handle open types\n"; assert false

let mk_dispatch loc decls_in_env decl =
  let here elt = L.mkloc elt loc in
  let name = decl.ptype_name.txt in
  let body = generate_dispatch_body loc decls_in_env decl in
  let expression = [%expr
    fun env states element -> [%e body]
  ]
  in
  H.Vb.mk
    ~loc
    (H.Pat.var ~loc (here name))
    expression


let combine_all decls_in_env decls =
  let loc = Location.none in
  (* The "dispatch" function *)
  let dispatch = global_dispatcher loc decls
  (* The auxiliary dispatch functions *)
  and aux_dispatches = List.map (mk_dispatch loc decls_in_env) decls in

  [
    H.Str.value
      ~loc
      Recursive
      (dispatch :: aux_dispatches @ static_bindings)
  ]
