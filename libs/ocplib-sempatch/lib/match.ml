open Std_utils

open Option.Infix

module M = StringMap
module V = Variable

type t = Variable.t M.t

let get key vars =
  try
    M.find key vars |> Option.some
  with
    Not_found -> None

let get_expr key vars =
  get key vars
  >>= (function
      | V.Expression e -> Some e
      | V.Ident i -> Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident i)) |> Option.some
    )

let get_ident key vars =
  get key vars
  >>= (function
      | V.Ident p -> Some p
      | _ -> None
    )

let is_defined_ident key vars = Option.is_some (get_ident key vars)

let add_expr name value vars = M.add name (V.Expression value) vars
let add_ident name value vars = M.add name (V.Ident value) vars

let merge m1 m2 =
  StringMap.merge (fun _ -> Option.merge_sup (fun _ x -> x)) m1 m2

let empty = M.empty
