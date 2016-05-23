module A = Automaton
module AE = Ast_element
open Parsetree
open Std_utils

let final () = A.{
    transitions = [];
    final = true;
  }

let make_report s =
  A.{ s with
      transitions = List.map (fun (_, f) -> true, f) s.transitions;
    }


let add_elements_from s1 s2 =
  let open A in
  s1.transitions <- s1.transitions @ s2.transitions;
  s1.final <- s1.final || s2.final;
  s1

let ignore_meta f m y = List.map (fun elt -> elt, m) (f y)

let basic_state f = A.{
    transitions = [
      false,
      ignore_meta @@ f
    ];
    final = false;
  }

let match_expr f = basic_state @@ function
  | AE.Expression e -> f e
  | _ -> []

let match_pat f = basic_state @@ function
  | AE.Pattern e -> f e
  | _ -> []

let match_var var =
  match_expr @@ function
  | { pexp_desc = Pexp_ident ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [[final ()]]
  | _ -> []

and match_const const =
  match_expr @@ function
  | { pexp_desc = Pexp_constant cst; _ }
    when cst = const ->
    [[final ()]]
  | _ -> []

and match_apply ref_lbl left right =
  match_expr @@ function
  | { pexp_desc = Pexp_apply (_, [lbl, _ ]); _}
    when ref_lbl = lbl ->
    [[left; right]]
  | _ -> []

and match_ifthenelse eif ethen eelse =
  match_expr @@ function
  | { pexp_desc = Pexp_ifthenelse _; _ } ->
    [[eif; ethen; eelse]]
  | _ -> []

and match_construct id sub_state =
  match_expr @@ function
  | { pexp_desc = Pexp_construct ({ Asttypes.txt = ast_id; _ }, _); _}
    when id = ast_id ->
    [[sub_state]]
  | _ -> []

and match_tuple sub_state =
  match_expr @@ function
  | { pexp_desc = Pexp_tuple _; _ } ->
    [[sub_state]]
  | _ -> []

let match_let isrec bindings_states expr_state =
  match_expr @@ function
  | { pexp_desc = Pexp_let (rec_flag, _, _); _ } ->
    if rec_flag = isrec then
      [[expr_state; bindings_states]]
    else
      []
  | _ -> []

let match_fun lbl_ref default_arg_s arg_s body_s =
  match_expr @@ function
  | { pexp_desc = Pexp_fun (lbl, _, _, _); _ }
    when lbl = lbl_ref ->
    [[default_arg_s; arg_s; body_s]]
  | _ -> []

let match_function cases_s =
  match_expr @@ function
  | { pexp_desc = Pexp_function _; _ } ->
    [[cases_s]]
  | _ -> []

let match_match expr_s cases_s =
  match_expr @@ function
  | { pexp_desc = Pexp_match _; _ } ->
    [[expr_s; cases_s]]
  | _ -> []

let match_try expr_s cases_s =
  match_expr @@ function
  | { pexp_desc = Pexp_try _; _ } ->
    [[expr_s; cases_s]]
  | _ -> []

let match_sequence left_s right_s =
  match_expr @@ function
  | { pexp_desc = Pexp_sequence _; _ } ->
    [[left_s; right_s]]
  | _ -> []

let match_field expr_s field_patch =
  match_expr @@ function
  | { pexp_desc = Pexp_field (_, { Asttypes.txt = field; _ }); _ }
    when field = field_patch ->
    [[expr_s]]
  | _ -> []

let match_record fields_s model_s =
  match_expr @@ function
  | { pexp_desc = Pexp_record _; _ } ->
    [[fields_s; model_s]]
  | _ -> []

let match_open isoverride name_ref expr_s =
  match_expr @@ function
  | { pexp_desc = Pexp_open (override, { Asttypes.txt = name; _ }, _ ); _ }
    when override = isoverride
      && name = name_ref ->
    [[expr_s]]
  | _ -> []

let match_variant lbl_ref body_s =
  match_expr @@ function
  | { pexp_desc = Pexp_variant (lbl, _ ); _ }
    when lbl = lbl_ref ->
    [[body_s]]
  | _ -> []

and match_assert body_s =
  match_expr @@ function
  | { pexp_desc = Pexp_assert _; _ } ->
    [[body_s]]
  | _ -> []

and match_var_pattern var =
  match_pat @@ function
  | { ppat_desc = Ppat_var ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [[final ()]]
  | _ -> []

and match_pat_construct id_ref sub_pat_state =
  match_pat @@ function
  | { ppat_desc = Ppat_construct ({Asttypes.txt = id; _ }, _); _ }
    when id_ref = id ->
    [[sub_pat_state]]
  | _ -> []

and match_value_binding pattern expr =
  basic_state @@ fun _ ->
  [[pattern; expr]]

and match_case lhs guard rhs =
  basic_state @@ function
  | AE.Case _ ->
  [[lhs; guard; rhs]]
  | _ -> []

and match_record_field id_ref expr_s =
  basic_state @@ function
  | AE.LPAR_Longident_DOT_t_loc_AND__expression_RPAR ({ Asttypes.txt = id; _ }, _)
    when id = id_ref ->
    [[expr_s]]
  | _ -> []

and match_any_expr id = A.{
    transitions = [
      false,
      fun meta ast_elt ->
        match ast_elt with
        | AE.Expression expr ->
        [
          [final ()],
          {meta with
           Match.substitutions =
             Substitution.add_expr
               id
               expr
               meta.Match.substitutions
          }
        ]
        | _ -> []
    ];
    final = false;
  }

and match_any_pattern id = A.{
    transitions = [
      false,
      fun meta ast_elt ->
        match ast_elt with
        | AE.Pattern pat ->
        [
          [final ()],
          {meta with
           Match.substitutions =
             Substitution.add_pattern
               id
               pat
               meta.Match.substitutions
          }
        ]
        | _ -> []
    ];
    final = false;
  }

let catchall () =
  let open A in
  let state =
    {
      transitions = [];
      final = false;
    }
  in
  let dispatch_list length =
    let rec list_make = function
      | 0 -> []
      | n when n > 0 -> () :: list_make (n-1)
      | _ -> raise (Invalid_argument "list_make")
    in
    let l = list_make length
    in
    List.mapi (fun idx _ ->
        List.mapi (fun jdx _ ->
            if idx = jdx then state else final ()
          )
          l
      )
      l
  in
  let catchall_expressions = function
    | Pexp_construct _
    | Pexp_function _
    | Pexp_tuple _
      ->
       dispatch_list 1

    | Pexp_let _
    | Pexp_apply _
    | Pexp_sequence _
    | Pexp_match _
      -> dispatch_list 2

    | Pexp_ifthenelse _
    | Pexp_fun _
      -> dispatch_list 3

    | _ -> []
  and catchall_str_items = function
    | Pstr_eval _
    | Pstr_value _ ->
       dispatch_list 1
    | _ -> []

  and catchall_patterns = function
    | Ppat_construct _
        -> dispatch_list 1
    | _ -> []
  in
    state.transitions <-
    [
      false,
      ignore_meta @@ function
      | AE.Expression e -> catchall_expressions e.pexp_desc
      | AE.Structure_item i -> catchall_str_items i.pstr_desc
      | AE.Pattern p -> catchall_patterns p.ppat_desc
      | AE.Pattern_option _
      | AE.Expression_option _
        -> dispatch_list 1
      | AE.Structure _
      | AE.Value_binding_list _
      | AE.Value_binding _
      | AE.Case_list _
      | AE.Expression_list _
        -> dispatch_list 2
      | AE.Case _
        -> dispatch_list 3
      | _ -> []
    ];
  state

let trash () = A.{
    transitions = [];
    final = false;
  }

let rec from_expr metas expr =
  match expr.pexp_desc with
  | Pexp_ident { Asttypes.txt = Longident.Lident id; _ }
    when List.mem id metas
    ->
    match_any_expr id
  | Pexp_ident { Asttypes.txt = id; _ } ->
    match_var id
  | Pexp_constant c ->
    match_const c
  | Pexp_construct ({ Asttypes.txt = id; _ }, expr_opt) ->
    match_construct id (from_maybe_expr metas expr_opt)
  | Pexp_tuple exprs ->
    match_tuple (from_expr_list metas exprs)
  | Pexp_apply (f, [lbl, arg]) ->
    match_apply lbl (from_expr metas f) (from_expr metas arg)
  | Pexp_let (isrec, bindings, expr) ->
    match_let isrec (from_value_bindings metas bindings) (from_expr metas expr)
  | Pexp_ifthenelse (eif, ethen, eelse) ->
    match_ifthenelse
      (from_expr metas eif)
      (from_expr metas ethen)
      (from_maybe_expr metas eelse)
  | Pexp_sequence (e1, e2) ->
    match_sequence (from_expr metas e1) (from_expr metas e2)
  | Pexp_function cases ->
    match_function (from_cases metas cases)
  | Pexp_match (e, cases) ->
    match_match (from_expr metas e) (from_cases metas cases)
  | Pexp_field (e, { Asttypes.txt = field; _}) ->
    match_field (from_expr metas e) field
  | Pexp_open (override, { Asttypes.txt = module_name; _ }, expr) ->
    match_open override module_name (from_expr metas expr)
  | Pexp_variant (name, body) ->
    match_variant name (from_maybe_expr metas body)
  | Pexp_try (expression, cases) ->
    match_try (from_expr metas expression) (from_cases metas cases)
  | Pexp_assert expr ->
    match_assert (from_expr metas expr)
  (* | Pexp_record (fields, model) -> *)
  (*   match_record *)
  (*     (from_record_fields metas fields) *)
  (*     (from_maybe_expr metas model) *)
  | Pexp_fun (lbl, default_arg, arg, body) ->
     match_fun
       lbl
       (from_maybe_expr metas default_arg)
       (from_pattern metas arg)
       (from_expr metas body)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_inside"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    add_elements_from (catchall ()) (from_expr metas e)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_report"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    (from_expr metas e |> make_report)
  | _ ->
    Printf.eprintf "Unimplemented expression %s\n"
      Failure.(to_string (Non_implemented expr.pexp_loc));
    trash ()
  (*   raise Failure.(SempatchException (Non_implemented expr.pexp_loc)) *)

and from_maybe_expr metas = function
  | None ->
    begin
      basic_state @@ function
      | AE.Expression_option None -> [[final ()]]
      | _ -> []
    end
  | Some expr ->
    begin
      let next_state = [[from_expr metas expr]] in
      basic_state @@ function
      | AE.Expression_option (Some _) -> next_state
      | _ -> []
    end

and from_expr_list metas exprs =
  let aux accu expr =
    basic_state @@ fun _ -> [[from_expr metas expr; accu]]
  and terminal = basic_state @@ function
    | AE.Expression_list [] -> [[final ()]]
    | _ -> []
  in
  List.fold_left aux terminal (List.rev exprs)

and from_case metas case =
  match_case
    (from_pattern metas case.pc_lhs)
    (from_maybe_expr metas case.pc_guard)
    (from_expr metas case.pc_rhs)

and from_cases metas cases =
  let aux accu case =
    basic_state @@ function
    | AE.Case_list _ -> [[from_case metas case; accu]]
    | _ -> []
  and terminal = basic_state @@ fun _ -> [[final ()]]
  in
  List.fold_left aux terminal (List.rev cases)

and from_pattern metas pattern =
  match pattern.ppat_desc with
  | Ppat_var { Asttypes.txt = id; _ }
    when List.mem id metas
    ->
    match_any_pattern id
  | Ppat_var { Asttypes.txt = id; _ } ->
    match_var_pattern id
  | Ppat_construct ({ Asttypes.txt = constr; _ }, sub_pat_opt)
    ->
    match_pat_construct constr (from_pattern_opt metas sub_pat_opt)
  | _ ->
    Printf.eprintf "Unimplemented pattern %s\n"
      Failure.(to_string (Non_implemented pattern.ppat_loc));
    trash ()
    (* raise Failure.(SempatchException (Non_implemented pattern.ppat_loc)) *)

and from_pattern_opt metas = function
  | None ->
    begin
      basic_state @@ function
      | AE.Pattern_option None -> [[final ()]]
      | _ -> []
    end
  | Some expr ->
    begin
      let next_state = [[from_pattern metas expr]] in
      basic_state @@ function
      | AE.Pattern_option (Some _) -> next_state
      | _ -> []
    end

and from_value_binding metas { pvb_expr; pvb_pat; _ } =
  match_value_binding
    (from_pattern metas pvb_pat)
    (from_expr metas pvb_expr)

and from_value_bindings metas vbs =
  let aux accu vb =
    basic_state @@ fun _ -> [[from_value_binding metas vb; accu]]
  and terminal = basic_state @@ fun _ -> [[final ()]]
  in
  List.fold_left aux terminal (List.rev vbs)

(* and from_record_field metas ({ Asttypes.txt = lbl; _ }, expr) = *)
(*   match_record_field lbl (from_expr metas expr) *)

(* and from_record_fields metas fields = *)
(*   let aux accu field = *)
(*     basic_state @@ fun _ -> [[from_record_field metas field; accu]] *)
(* and terminal = basic_state @@ function *)
(*   | AE.Record_fields [] -> [[final ()]] *)
(*   | _ -> [] *)
(*   in *)
(*   List.fold_left aux terminal (List.rev fields) *)
