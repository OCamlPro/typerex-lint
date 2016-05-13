open Parsetree
open Std_utils
module A = Automaton

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
    List.map (fun loc -> Builder.final, { merged_matches with Match.location = Some loc })
      locations

let rec apply' : type a. A.meta_info -> a A.t -> a -> (a A.t * A.meta_info) list
    = fun env state node ->
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
           (trans state env node)
        )
        state.A.transitions
    in
    dispatch new_states node

and apply2 :
  type a. a A.state_bundle -> A.meta_info -> a ->
  (a A.t * A.meta_info) list =
  fun state_bun env expr ->
    match state_bun, expr with
    | A.Final, _ -> [Builder.final, env]
    | A.Expr (A.Apply (s1, s2)), {
        pexp_desc = Pexp_apply (e1, ["", e2]);
        _
      } ->
      List.product_bind both
        (apply' (setloc e1.pexp_loc env) s1 (e1))
        (apply' (setloc e2.pexp_loc env) s2 (e2))
    | A.Expr (A.Let (bindings_state, expr_state)), {
        pexp_desc = Pexp_let (_, bindings, expr);
        _
      } ->
      let bindings_final_states =
        let states_list = List.map2 (fun binding_state binding ->
            apply' (setloc binding.pvb_loc env) binding_state binding)
          bindings_state bindings
        in match states_list with
        | [] -> []
        | hd::tl -> List.fold_left (List.product_bind both) hd tl
      in
      List.product_bind both
        (apply' (setloc expr.pexp_loc env) expr_state (expr))
        bindings_final_states
    | A.Pattern _, {
        ppat_loc = l;
        _
      } -> [Builder.final, setloc l env]
    | A.Value_binding { A.vb_pat; vb_expr; }, {
        pvb_pat = pat;
        pvb_expr = expr;
        _
      } ->
      List.product_bind both
        (apply' (setloc expr.pexp_loc env) vb_expr expr)
        (apply' (setloc pat.ppat_loc env) vb_pat pat)
    | _ -> []

and dispatch : type a. (a A.state_bundle * A.meta_info) list
  -> a -> (a A.t * A.meta_info) list =
  fun state_bundles expr ->
  List.bind (fun (state_bun, env) -> apply2 state_bun env expr) state_bundles

let apply name state expr =
  let results = apply'
      (Match.mk name Substitution.empty None expr.pexp_loc)
      state expr
  in
  results
