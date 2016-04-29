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

let apply_to_2_list f = function
  | [ x; y ] -> f x y
  | _ -> raise TypeError

let equiv_ast t1 t2 =
  match t1, t2 with
  | Expr e1, Expr e2 ->
    begin
      let open Ast_maybe_mapper2 in
      let default_mapper = Ast_maybe_mapper2.mk (fun () () -> ()) in
      let mapper = {
        default_mapper with
        expr = fun self () ~patch ~expr ->
          match expr.pexp_desc, patch.pexp_desc with
          | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Ok (expr, ())
          | Pexp_constant i, Pexp_constant j when i = j -> Ok (expr, ())
          | _, _ -> default_mapper.expr self () ~expr ~patch
      }
      in
      match mapper.expr mapper () ~expr:e1 ~patch:e2 with
      | Ok _ -> Bool true
      | Error _ -> Bool false
    end
  | _, _ -> raise TypeError

let functions = [
  "(=)", apply_to_2_list equiv_ast
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
