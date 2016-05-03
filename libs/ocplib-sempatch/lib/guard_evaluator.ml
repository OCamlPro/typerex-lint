open Std_utils
open Parsetree
open Guard

type t =
  | Expr of Parsetree.expression
  | Bool of bool

type fn = string * (t list -> t) (* name, fun *)

exception Undefined_var of string
exception Undefined_function of string
exception TypeError

let constants = [
  "true", Bool true;
  "false", Bool false;
]

let find_var var_name env =
  try
    List.assoc var_name constants
  with
    Not_found ->
    match Substitution.get_expr var_name env with
    | Some x -> Expr x
    | None -> raise (Undefined_var var_name)

let apply_to_bool f args =
  let unwrap_bool = function
    | Bool b -> b
    | Expr _ -> raise TypeError
  in
  f (List.map unwrap_bool args)

let apply_to_exprs f args =
  let unwrap_expr = function
    | Bool _ -> raise TypeError
    | Expr e -> e
  in
  f (List.map unwrap_expr args)

let apply_to_1 f = function
  | [x] -> f x
  | _ -> raise TypeError

let apply_to_2 f = function
  | [ x; y ] -> f x y
  | _ -> raise TypeError

let bool f x = Bool (f x)
let expr f x = Expr (f x)

let equiv_ast = apply_to_exprs @@ apply_to_2 @@ fun e1 e2 ->
  let open Ast_maybe_mapper2 in
  let default_mapper = Ast_maybe_mapper2.mk (fun () () -> ()) in
  let mapper = {
    default_mapper with
    expr = fun self () ~patch ~expr ->
      match expr.pexp_desc, patch.pexp_desc with
      | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt ->
        Ok (expr, ())
      | Pexp_constant i, Pexp_constant j when i = j -> Ok (expr, ())
      | _, _ -> default_mapper.expr self () ~expr ~patch
  }
  in
  match mapper.expr mapper () ~expr:e1 ~patch:e2 with
  | Ok _ -> true
  | Error _ -> false

let (&&&) = apply_to_bool @@ apply_to_2 @@ (fun x y -> x&&y)
let (|||) = apply_to_bool @@ apply_to_2 @@ (fun x y -> x||y)

let is_variable = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_ident _ -> true
  | _ -> false

let is_constant = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_constant _ -> true
  | Pexp_construct (_, None) -> true
  | _ -> false

let is_integer_lit = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_constant (Asttypes.Const_int _)
  | Pexp_constant (Asttypes.Const_int32 _)
  | Pexp_constant (Asttypes.Const_int64 _)
  | Pexp_constant (Asttypes.Const_nativeint _)
    -> true
  | _ -> false

let is_string_lit = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_constant (Asttypes.Const_string _)
    -> true
  | _ -> false

let is_bool_lit = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_construct (id, None)
    when List.mem
        id.Asttypes.txt
        [ Longident.Lident "true"; Longident.Lident "false" ]
    -> true
  | _ -> false

let functions = [
  "(=)", bool @@ equiv_ast;
  "(&&)", bool @@  (&&&);
  "(||)", bool @@ (|||);
  "is_variable", bool @@ is_variable;
  "is_constant", bool @@ is_constant;
  "is_leaf", bool @@ (fun x -> (is_variable x || is_constant x));
  "is_integer_lit", bool @@ is_integer_lit;
  "is_string_lit", bool @@ is_string_lit;
  "is_bool_lit", bool @@ is_bool_lit;
]

let rec eval_ env = function
  | Variable v -> find_var v env
  | Apply (f, args) ->
    let fn =
      try List.assoc f functions with Not_found -> raise (Undefined_function f)
    in
    fn (List.map (eval_ env) args)

let eval env guard =
  match eval_ env guard with
  | Bool b -> b
  | Expr _ -> raise TypeError

let eval_union env guards =
  List.fold_left (fun accu guard -> accu && eval env guard) true guards
