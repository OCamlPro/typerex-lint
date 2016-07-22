open Std_utils
open Parsetree
open Guard

type t =
  | Expr of Parsetree.expression
  | Bool of bool
  | Int of int

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
    begin
      match Substitution.get_expr var_name env with
      | Some x -> Expr x
      | None -> raise (Undefined_var var_name)
    end

let apply_to_bool f args =
  let unwrap_bool = function
    | Bool b -> b
    | _ -> raise TypeError
  in
  f (List.map unwrap_bool args)

let apply_to_exprs f args =
  let unwrap_expr = function
    | Expr e -> e
    | _ -> raise TypeError
  in
  f (List.map unwrap_expr args)

let apply_to_1 f = function
  | [x] -> f x
  | _ -> raise TypeError

let apply_to_2 f = function
  | [ x; y ] -> f x y
  | _ -> raise TypeError

let bool f x = Bool (f x)

let equiv_ast = apply_to_exprs @@ apply_to_2 @@ fun e1 e2 ->
  let patch = Parsed_patches.preprocess Parsed_patches.{
      unprocessed_header =
        { Parsed_patches.void_header with name = "guard"};
      unprocessed_body = e1;
    }
  in
  match
    Eval.apply "" (Parsed_patches.get_body patch)
      (Ast_element.Element.Expression e2)
  with
  | [] -> false
  | _ -> true

let (&&&) = apply_to_bool @@ apply_to_2 @@ (fun x y -> x&&y)
let (|||) = apply_to_bool @@ apply_to_2 @@ (fun x y -> x||y)
let (guard_not) = apply_to_bool @@ apply_to_1 @@ not

let is_variable = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_ident _ -> true
  | _ -> false

let is_constant = apply_to_exprs @@ apply_to_1 @@ fun e ->
  match e.pexp_desc with
  | Pexp_constant _ -> true
  | Pexp_construct (_, None) -> true
  | _ -> false

let is_int_in_range = fun args ->
  match args with
  | [Expr e; Int min; Int max] ->
    begin
      let int_expr =
        match e.pexp_desc with
        | Pexp_constant (Asttypes.Const_int i) -> Some i
        | _ -> None
      in
      Option.fold (fun _ i -> min <= i && max >= i) false int_expr
    end
  | _ -> false

let is_in_range = fun args ->
  match args with
  | [Expr e; Int min; Int max] ->
    begin
      let int_expr =
        match e.pexp_desc with
        | Pexp_constant (Asttypes.Const_int i) -> Some i
        | Pexp_constant (Asttypes.Const_int32 i) -> Some (Int32.to_int i)
        | Pexp_constant (Asttypes.Const_int64 i) -> Some (Int64.to_int i)
        | Pexp_constant (Asttypes.Const_nativeint i) ->
          Some (Nativeint.to_int i)
        | _ -> None
      in
      Option.fold (fun _ i -> min <= i && max >= i) false int_expr
    end
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

let functions : fn list = [
  "(=)", bool @@ equiv_ast;
  "(&&)", bool @@  (&&&);
  "(||)", bool @@ (|||);
  "not", bool @@ guard_not;
  "is_variable", bool @@ is_variable;
  "is_constant", bool @@ is_constant;
  "is_leaf", bool @@ (fun x -> is_variable x || is_constant x);
  "is_integer_lit", bool @@ is_integer_lit;
  "is_string_lit", bool @@ is_string_lit;
  "is_bool_lit", bool @@ is_bool_lit;
  "is_int_in_range", bool @@ is_int_in_range;
  "is_in_range", bool @@ is_in_range;
]

let rec eval_ env = function
  | Variable v -> find_var v env
  | Apply (f, args) ->
    let fn =
      try List.assoc f functions with Not_found -> raise (Undefined_function f)
    in
    fn (List.map (eval_ env) args)
  | Litt_integer i -> Int i

let eval env guard =
  match eval_ env guard with
  | Bool b -> b
  | _ -> raise TypeError

let eval_union env guards =
  List.fold_left (fun accu guard -> accu && eval env guard) true guards
