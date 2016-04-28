(* open Std_utils *)

module M = Match

type t = {
  current_match : Match.t;
  matches : (Match.t * Location.t) list;
}


let map_current f x = { x with current_match = f x.current_match }
let set_current new_match = map_current (fun _ -> new_match)
let add_expr name value = map_current (M.add_expr name value)
let add_ident name value = map_current (M.add_ident name value)

let map_matches f x = { x with matches = f x.matches }
let set_matches m = map_matches (fun _ -> m)

let get key env = M.get key env.current_match
let get_expr key env = M.get_expr key env.current_match
let get_ident key env = M.get_ident key env.current_match

let empty = { current_match = M.empty; matches = []; }

let merge e1 e2 = {
  current_match = M.merge e1.current_match e2.current_match;
  matches = List.append e1.matches e2.matches;
}
