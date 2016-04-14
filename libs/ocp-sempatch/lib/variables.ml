open Std_utils
open Parsetree

open Option.Infix

type v =
  | Expression of expression_desc
  | Ident of string

module M = Map.Make(String)

type t = v M.t

let get key universe =
  try
    M.find key universe |> Option.some
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

let mem = M.mem

let is_defined_ident key universe = Option.is_some (get_ident key universe)

let add = M.add
