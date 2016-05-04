open Std_utils

open Option.Infix

module M = StringMap
module AE = Ast_element

type t = AE.t M.t

let get key vars =
  try
    M.find key vars |> Option.some
  with
    Not_found -> None

let get_expr key vars =
  get key vars
  >>= (function
      | AE.Expression e -> Some e
      | AE.Ident i ->
        Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident i))
                      |> Option.some
    )

let get_ident key vars =
  get key vars
  >>= (function
      | AE.Ident p -> Some p
      | _ -> None
    )

(* let is_defined_ident key vars = Option.is_some (get_ident key vars) *)

let add_expr name value vars = M.add name (AE.Expression value) vars
let add_ident name value vars = M.add name (AE.Ident value) vars

let merge m1 m2 =
  StringMap.merge (fun _ -> Option.merge_sup (fun _ x -> x)) m1 m2

let to_list = M.bindings

let empty = M.empty
