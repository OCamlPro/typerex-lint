(* open Std_utils *)

module S = Substitution

type t = {
  current_match : S.t;
  matches : Match.t list;
}


let map_current f x = { x with current_match = f x.current_match }
let set_current new_match = map_current (fun _ -> new_match)
let add_expr name value = map_current (S.add_expr name value)
let add_ident name value = map_current (S.add_ident name value)

let map_matches f x = { x with matches = f x.matches }
let set_matches m = map_matches (fun _ -> m)

let get_expr key env = S.get_expr key env.current_match
let get_ident key env = S.get_ident key env.current_match

let empty = { current_match = S.empty; matches = []; }

let merge e1 e2 = {
  current_match = S.merge e1.current_match e2.current_match;
  matches = List.append e1.matches e2.matches;
}
