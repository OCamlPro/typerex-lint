open Std_utils

open Option.Infix

module M = StringMap
module AE = Ast_element.Element

type t = AE.t M.t

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
    Not_found ->
    Messages.warn "couldn't find %s\n" key;
    None

let get_expr key vars =
  get key vars
  >>= (function
      | AE.Expression e -> Some e
      | AE.Pattern p -> pat_to_expr p
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

let add_expr name value vars =
  Messages.debug "adding %s to the variables as the expression '%s'\n"
    name
    (Pprintast.expression Format.str_formatter value;
     Format.flush_str_formatter ())
  ;
  M.add
    name
    (AE.Expression value)
    vars

let add_ident name value vars = M.add name (AE.String value) vars
let add_pattern name value vars = M.add name (AE.Pattern value) vars

let merge m1 m2 =
  StringMap.merge (fun _ -> Option.merge_sup (fun _ x -> x)) m1 m2

let to_list = M.bindings

let empty = M.empty
