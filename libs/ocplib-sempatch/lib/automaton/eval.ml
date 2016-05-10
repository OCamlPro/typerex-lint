open Parsetree
open Std_utils
module A = Automaton

let both (s1, l1) (s2, l2) =
  if not A.(s1.final && s2.final) then
    []
  else
    match l1, l2 with
    | Some x, Some y when x != y ->
      [s1, l1; s2, l2]
    | Some _, _ ->
      [s1, l1]
    | None, Some _ ->
      [s2, l2]
    | _ -> []

let rec apply' begin_of_match state expr =
  if state.A.final then
    [state, begin_of_match]
  else
    let new_states = List.bind
        (fun (update_loc, trans) ->
           List.map
             (fun x ->
                (if update_loc then Some expr.pexp_loc else begin_of_match), x
             )
             (trans state expr)
        )
        state.A.transitions
    in
    map2 new_states expr

and map2 states_expr expr =
  List.bind (fun (loc, state_expr) ->
      match state_expr, expr with
      | [s1; s2], { pexp_desc = Pexp_let (_, [ { pvb_expr = e1; _ }], e2); _ }
      | [s1; s2], { pexp_desc = Pexp_apply (e1, ["", e2]); _ } ->
        List.product_bind both (apply' loc s1 e1) (apply' loc s2 e2)
      | [l], _ -> [l, loc]
      | _ -> []
    )
    states_expr

let apply state expr =
  let results = apply' None state expr in
  List.iter
    (
      function
      | (state, Some loc) ->
        let loc_begin = loc.Location.loc_start in
        if state.A.final then
          Printf.printf "Match at %i:%i\n"
            Lexing.(loc_begin.pos_lnum)
            Lexing.(loc_begin.pos_cnum - loc_begin.pos_bol)
        else assert false
      | _ -> ()
    )
    results;
  results
