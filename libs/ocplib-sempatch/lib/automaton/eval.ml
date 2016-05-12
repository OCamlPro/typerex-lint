open Parsetree
open Std_utils
module A = Automaton

let both (s1, l1) (s2, l2) =
  if not A.(s1.final && s2.final) then
    []
  else
    let locations =
      List.bind (Option.to_list)
        [Match.get_location l1; Match.get_location l2]
    and merged_matches = {
      l1 with
      Match.substitutions = Substitution.merge
          (Match.get_substitutions l1)
          (Match.get_substitutions l2)
      ;
    }
    in
    List.map (fun loc -> s1, { merged_matches with Match.location = Some loc })
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
        pexp_loc = l;
        _
      } ->
      let env = Match.set_current_location l env in
      List.product_bind both
        (apply' env s1 (e1))
        (apply' env s2 (e2))
    | A.Pattern _, {
        ppat_loc = l;
        _
      } -> [Builder.final, Match.set_current_location l env]
    | _ -> [Builder.final, env]

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
