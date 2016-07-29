open Std_utils
module A = Automaton.A
module T = Ast_element.Element

let semilazy_product_bind f l1 ll2 = match l1 with
  | [] -> []
  | _ -> List.product_bind f l1 (Lazy.force ll2)


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
          fun loc -> Automaton.final (),
                     { merged_matches with Match.location = loc }
        )
          locations
      with
      | [] -> [Automaton.final (), merged_matches]
      | l -> l

[%%create_eval]

let apply name state elt =
  apply'
    (Match.mk name Substitution.empty None Location.none)
    state elt
