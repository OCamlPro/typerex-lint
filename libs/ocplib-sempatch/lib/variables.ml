open Std_utils
open Parsetree

open Option.Infix

type v =
  | Expression of expression_desc
  | Ident of string

module M = MyStringMap

type env = v M.t

type t = {
  env : env;
  matches : (env * Location.t) list;
}

let set_env e v = { v with env = e }

let map_env f x = { x with env = f x.env }
let map_pos f x = { x with matches = f x.matches }

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
  env = M.merge (fun _ -> Option.merge_sup Misc.const) v1.env v2.env;
  matches = v1.matches @ v2.matches;
}

let empty = { env = M.empty; matches = []; }
