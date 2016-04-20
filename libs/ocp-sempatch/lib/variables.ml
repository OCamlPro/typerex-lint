open Std_utils
open Parsetree

open Option.Infix

type v =
  | Expression of expression_desc
  | Ident of string

module M = Map.Make(String)

type t = {
  env : v M.t;
  matches_positions : Location.t list;
}

let set_env e v = { v with env = e }

let map_env f x = { x with env = f x.env }
let map_pos f x = { x with matches_positions = f x.matches_positions }

let add_match loc = map_pos (List.cons loc)
let set_loc loc = map_pos (fun _ -> loc)

let add_env k elt = map_env (M.add k elt)

let get key universe =
  try
    M.find key universe.env |> Option.some
  with
    Not_found -> None

let get_expr key universe =
  get key universe
  >>= (function
      | Expression e -> Some e
      | Ident i -> Pexp_ident (Location.mknoloc (Longident.Lident i)) |> Option.some
    )

let get_ident key universe =
  get key universe
  >>= (function
      | Ident p -> Some p
      | _ -> None
    )

let mem k x = M.mem k x.env

let is_defined_ident key universe = Option.is_some (get_ident key universe)

let merge v1 v2 = {
  env = M.merge (fun _ -> Misc.const) v1.env v2.env;
  matches_positions = v1.matches_positions @ v2.matches_positions;
}

let empty = { env = M.empty; matches_positions = []; }
