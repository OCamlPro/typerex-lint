open Parsetree
open Std_utils
module A = Automaton
module AE = Ast_element

let setloc = Match.set_current_location

let both
  = fun (s1, l1) (s2, l2) ->
  if not A.(s1.final && s2.final) then
    []
  else
    let locations =
      List.sort_uniq compare
        [Match.get_location l1; Match.get_location l2]
    and merged_matches = {
      l2 with
      Match.substitutions = Substitution.merge
          (Match.get_substitutions l1)
          (Match.get_substitutions l2)
      ;
    }
    in
    match
    List.map (
      fun loc -> Builder.final (),
                 { merged_matches with Match.location = loc }
      )
      locations
    with
    | [] -> [Builder.final (), merged_matches]
    | l -> l

let rec apply' = fun env state node ->
  if state.A.final then
    [state, env]
  else
    let new_states = List.bind
        (fun (update_loc, trans) ->
           let new_loc =
             if update_loc then
               Some (Match.get_current_location env)
             else
               Match.get_location env
           in
           let env = { env with Match.location = new_loc } in
           (trans env node)
        )
        state.A.transitions
    in
    dispatch new_states node

and apply2 = fun state_bun env expr ->
  let sub_results =
    match state_bun, expr with
    | [single], _ when single.A.final ->
       [[single, env]]

    | _, AE.Expression { pexp_desc = d; _ } ->
       apply_expr state_bun env d

    | _, AE.Pattern {ppat_desc = d; _} ->
       apply_pat state_bun env d

    | [state], AE.Expression_opt (Some expr_opt) ->
       [apply' (setloc expr_opt.pexp_loc env) state (AE.Expression expr_opt)]

    | [expr_s; tl_s], AE.Expressions (expr::tl) ->
       [
         apply' (setloc expr.pexp_loc env) expr_s (AE.Expression expr);
         apply' env tl_s (AE.Expressions tl);
       ]

    | [state], AE.Pattern_opt (Some pat_opt) ->
       [apply' (setloc pat_opt.ppat_loc env) state (AE.Pattern pat_opt)]

    | [state], AE.Structure_item { pstr_desc = Pstr_eval (expr, _); _ } ->
       [apply' (setloc expr.pexp_loc env) state (AE.Expression expr)]

    | [state], AE.Structure_item { pstr_desc = Pstr_value (_, bindings); _ } ->
       [apply' env state (AE.Value_bindings bindings)]

    | [item_s; tl_s], AE.Structure (item::tl) ->
       [
         (apply' (setloc item.pstr_loc env) item_s
                 (AE.Structure_item item));
         (apply' env tl_s (AE.Structure tl));
       ]

    | [pat_s; expr_s;], AE.Value_binding {
                              pvb_pat = pat; pvb_expr = expr; _
                            } ->
       [
         (apply' (setloc pat.ppat_loc env) pat_s (AE.Pattern pat));
         (apply' (setloc expr.pexp_loc env) expr_s (AE.Expression expr));
       ]

    | [vb_s; tail_s], AE.Value_bindings (vb::tl) ->
       [
         (apply' (setloc vb.pvb_loc env) vb_s (AE.Value_binding vb));
         (apply' env tail_s (AE.Value_bindings tl));
       ]

    | [lhs_s; guard_s; rhs_s], AE.Case {
                                   pc_lhs; pc_guard; pc_rhs
                                 } ->
       [
         apply' (setloc pc_lhs.ppat_loc env) lhs_s (AE.Pattern pc_lhs);
         apply' env guard_s (AE.Expression_opt pc_guard);
         apply' (setloc pc_rhs.pexp_loc env) rhs_s (AE.Expression pc_rhs);
       ]

    | [case_s; tl_s], AE.Cases (case::tl) ->
       [
         (apply' env case_s (AE.Case case));
         (apply' env tl_s (AE.Cases tl));
       ]

    | [val_s], AE.Record_field (_, value) ->
      [apply' env val_s (AE.Expression value)]

    | [field_s; tl_s], AE.Record_fields (field::tl) ->
      [
        apply' env field_s (AE.Record_field field);
        apply' env tl_s (AE.Record_fields tl);
      ]

    | _ -> []
  in
  match sub_results with
  | [] -> []
  | hd::tl ->
    List.fold_left (List.product_bind both) hd tl

and apply_pat state_bun env pat_desc =
  match state_bun, pat_desc with
  | [s1], Ppat_construct (_, arg_opt) ->
    [apply' env s1 (AE.Pattern_opt arg_opt)]
  | _ ->
  [[Builder.final (), env]]

and apply_expr state_bun env exp_desc =
  match state_bun, exp_desc with
  | [s1; s2], Pexp_apply (e1, ["", e2]) ->
    [
      (apply' (setloc e1.pexp_loc env) s1 (AE.Expression e1));
      (apply' (setloc e2.pexp_loc env) s2 (AE.Expression e2))
    ]

  | [default_arg_s; arg_s; body_s],
    Pexp_fun (_lbl, default_arg, arg, body)
      ->
    [
      (apply' (setloc arg.ppat_loc env) arg_s (AE.Pattern arg));
      (apply' (setloc body.pexp_loc env) body_s (AE.Expression body));
      (apply' env default_arg_s (AE.Expression_opt default_arg));
    ]

  | [cases_s], Pexp_function cases ->
    [apply' env cases_s (AE.Cases cases)]

  | [expr_s; bindings_s], Pexp_let (_, bindings, expr) ->
    [
      (apply' (setloc expr.pexp_loc env) expr_s (AE.Expression expr));
      (apply' env bindings_s (AE.Value_bindings bindings));
    ]

  | [s_if; s_then; s_else], Pexp_ifthenelse (e_if, e_then, e_else) ->
    [
      (apply' (setloc e_then.pexp_loc env) s_then (AE.Expression e_then));
      (apply' (setloc e_if.pexp_loc env) s_if (AE.Expression e_if));
      (apply' (setloc e_then.pexp_loc env) s_else (AE.Expression_opt e_else));
    ]

  | [expr_s], Pexp_construct (_, expr) ->
    [apply' env expr_s (AE.Expression_opt expr)]

  | [fields_s; model_s], Pexp_record (fields, model) ->
    [
      apply' env fields_s (AE.Record_fields fields);
      apply' env model_s (AE.Expression_opt model);
    ]

  | [body_s], Pexp_tuple body ->
    [apply' env body_s (AE.Expressions body)]

  | [body_s], Pexp_field (expr, _) ->
    [apply' (setloc expr.pexp_loc env) body_s (AE.Expression expr)]

  | [s1; s2], Pexp_sequence (e1, e2) ->
    [
      apply' (setloc e1.pexp_loc env) s1 (AE.Expression e1);
      apply' (setloc e2.pexp_loc env) s2 (AE.Expression e2);
    ]

  | [body_s], Pexp_open (_, _, body) ->
    [apply' (setloc body.pexp_loc env) body_s (AE.Expression body)]

  | _ -> []


and dispatch = fun state_bundles expr ->
  List.bind (fun (state_bun, env) -> apply2 state_bun env expr) state_bundles

let apply name state elt =
  apply'
    (Match.mk name Substitution.empty None Location.none)
    state elt
