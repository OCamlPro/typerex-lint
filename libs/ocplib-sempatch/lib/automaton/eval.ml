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
      List.bind (Option.to_list)
        [Match.get_location l1; Match.get_location l2]
    and merged_matches = {
      l2 with
      Match.substitutions = Substitution.merge
          (Match.get_substitutions l1)
          (Match.get_substitutions l2)
      ;
    }
    in
    List.map (
      fun loc -> Builder.final (),
                 { merged_matches with Match.location = Some loc }
      )
      locations

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
  | [single], _ when single.A.final -> [[single, env]]

  | [s1; s2], AE.Expression {
      pexp_desc = Pexp_apply (e1, ["", e2]); _
    } ->
    [
      (apply' (setloc e1.pexp_loc env) s1 (AE.Expression e1));
      (apply' (setloc e2.pexp_loc env) s2 (AE.Expression e2))
    ]

  | [default_arg_state; arg_state; body_state], AE.Expression {
      pexp_desc = Pexp_fun (_lbl, default_arg, arg, body); _
    } ->
    [
      (apply' (setloc arg.ppat_loc env) arg_state (AE.Pattern arg));
      (apply' (setloc body.pexp_loc env) body_state (AE.Expression body));
      (apply' env default_arg_state (AE.Expression_opt default_arg));
    ]

  | [expr_state; bindings_state], AE.Expression {
      pexp_desc = Pexp_let (_, bindings, expr); _
    } ->
    [
      (apply' (setloc expr.pexp_loc env) expr_state (AE.Expression expr));
      (apply' env bindings_state (AE.Value_bindings bindings));
    ]

  | [s_if; s_then; s_else], AE.Expression {
      pexp_desc = Pexp_ifthenelse (e_if, e_then, e_else);
      _
    } ->
    [
      (apply' (setloc e_then.pexp_loc env) s_then (AE.Expression e_then));
      (apply' (setloc e_if.pexp_loc env) s_if (AE.Expression e_if));
      (apply' (setloc e_then.pexp_loc env) s_else (AE.Expression_opt e_else));
    ]

  | [expr_state], AE.Expression {
      pexp_desc = Pexp_construct (_, expr);
      _
    } ->
    [apply' env expr_state (AE.Expression_opt expr)]

  | [s1; s2], AE.Expression {
      pexp_desc = Pexp_sequence (e1, e2); _
    } ->
    [
      apply' (setloc e1.pexp_loc env) s1 (AE.Expression e1);
      apply' (setloc e2.pexp_loc env) s2 (AE.Expression e2);
    ]

  | _, AE.Pattern {ppat_loc = l; _} ->
    [[Builder.final (), setloc l env]]

  | [vb_pat; vb_expr;], AE.Value_binding {
      pvb_pat = pat; pvb_expr = expr; _
    } ->
    [
      (apply' (setloc pat.ppat_loc env) vb_pat (AE.Pattern pat));
      (apply' (setloc expr.pexp_loc env) vb_expr (AE.Expression expr));
    ]

  | [state], AE.Expression_opt (Some expr_opt) ->
    [apply' env state (AE.Expression expr_opt)]

  | [state], AE.Structure_item { pstr_desc = Pstr_eval (expr, _); _ } ->
    [apply' (setloc expr.pexp_loc env) state (AE.Expression expr)]

  | [state], AE.Structure_item { pstr_desc = Pstr_value (_, bindings); _ } ->
    [apply' env state (AE.Value_bindings bindings)]

  | [state_item; state_struct], AE.Structure (item::tl) ->
    [
      (apply' (setloc item.pstr_loc env) state_item (AE.Structure_item item));
      (apply' env state_struct (AE.Structure tl));
    ]

  | [state_vb; state_tail], AE.Value_bindings (vb::tl) ->
    [
      (apply' (setloc vb.pvb_loc env) state_vb (AE.Value_binding vb));
      (apply' env state_tail (AE.Value_bindings tl));
    ]

    | _ -> []
  in
  match sub_results with
  | [] -> []
  | hd::tl ->
    List.fold_left (List.product_bind both) hd tl

and dispatch = fun state_bundles expr ->
  List.bind (fun (state_bun, env) -> apply2 state_bun env expr) state_bundles

let apply name state expr =
  let results = apply'
      (Match.mk name Substitution.empty None Location.none)
      state (AE.Structure expr)
  in
  results
