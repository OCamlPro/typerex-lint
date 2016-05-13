open Std_utils

open Option.Infix

module M = StringMap
module AE = Ast_element

type t = AE.t M.t

let under_arg expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_apply (f, arg) -> Some (f, arg)
  | _ -> None

let uncurryfying_mapper =
  let open Ast_mapper in
  let open Parsetree in
  { default_mapper with
    expr = (fun self e ->
            match e.pexp_desc with
            | Pexp_apply (f, args)
              when List.exists
                  (fun (loc, _) -> loc.Location.txt = "__sempatch_uncurryfy")
                  f.pexp_attributes
              ->
              (
                match under_arg f with
                | Some (next_fun, next_arg) ->
                  self.expr self
                    { e with
                      pexp_desc = Pexp_apply (next_fun, next_arg @ args)
                    }
                | None -> default_mapper.expr self e
              )
            | _ -> default_mapper.expr self e
      );
  }

let postprocess = uncurryfying_mapper.Ast_mapper.expr uncurryfying_mapper

let get key vars =
  try
    M.find key vars |> Option.some
  with
    Not_found -> None

let get_expr key vars =
  let open Parsetree in
  get key vars
  >>= (function
      | AE.Expression e -> Some e
      | AE.Pattern { ppat_desc = Ppat_var { Asttypes.txt = i; _ }; _ }
      | AE.String i ->
        Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident i))
                      |> Option.some
      | _ -> None
    )

let get_ident key vars =
  get key vars
  >>= (function
      | AE.String p -> Some p
      | _ -> None
    )

(* let is_defined_ident key vars = Option.is_some (get_ident key vars) *)

let add_expr name value vars = M.add
    name
    (AE.Expression (postprocess value))
    vars
let add_ident name value vars = M.add name (AE.String value) vars
let add_pattern name value vars = M.add name (AE.Pattern value) vars

let merge m1 m2 =
  StringMap.merge (fun _ -> Option.merge_sup (fun _ x -> x)) m1 m2

let to_list = M.bindings

let empty = M.empty
