open Std_utils

open Option.Infix

module M = StringMap
module AE = Ast_element.Element

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

let rec pat_to_expr pat =
  let open Parsetree in
  let desc = match pat.ppat_desc with
    | Ppat_var { Asttypes.txt = v; _ } ->
      Some (Pexp_ident (Location.mknoloc (Longident.Lident v)))
    | Ppat_constant c -> Some (Pexp_constant c)
    | Ppat_construct (constr, Some arg) ->
      pat_to_expr arg
      >|= (fun arg_expr ->
          Pexp_construct (constr, Some arg_expr)
        )
    | Ppat_construct (constr, None) ->
      Some (Pexp_construct (constr, None))
    | Ppat_tuple sub_patterns ->
      List.flip_opt (List.map pat_to_expr sub_patterns)
      >|= (fun sub_expr ->
          Pexp_tuple sub_expr
        )
    | _ -> None
  in
  desc >|= (fun desc -> Ast_helper.Exp.mk desc)

let get key vars =
  try
    M.find key vars |> Option.some
  with
    Not_found -> None

let get_expr key vars =
  get key vars
  >>= (function
      (* | AE.Expression e -> Some e *)
      (* | AE.Pattern p -> pat_to_expr p *)
      (* | AE.String i -> *)
      (*   Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident i)) *)
      (*                 |> Option.some *)
      | _ -> None
    )

let get_ident key vars =
  get key vars
  >>= (function
      (* | AE.String p -> Some p *)
      | _ -> None
    )

(* let is_defined_ident key vars = Option.is_some (get_ident key vars) *)

let add_expr name value vars = M.add
    name
    (Ast_element.Element.Expression (postprocess value))
    vars

let add_ident _name _value _vars = assert false(* M.add name (AE.String value) vars *)
let add_pattern _name _value _vars = assert false(* M.add name (AE.Pattern value) vars *)

let merge m1 m2 =
  StringMap.merge (fun _ -> Option.merge_sup (fun _ x -> x)) m1 m2

let to_list = M.bindings

let empty = M.empty
