open Std_utils

type t =
  | Bool_op of bool op * t list
  | Predicate of Parsetree.expression op * caml_expr list

and caml_expr =
  | Variable of string

and 'a op = {
  arity : int;
  name : string;
  apply : 'a list -> bool
}

exception Undefined_var of string
exception GuardEvaluationError of string

let apply_to_2_list f = function
  | [ x; y ] -> f x y
  | _ -> assert false

let find name = List.find (fun x -> x.name = name)

let equiv_ast t1 t2 =
  let open Parsed_patches.Type in
  let patch = {
    header = Parsed_patches.void_header;
    body = t2;
  }
  in
  match Ast_pattern_matcher.apply patch t1 with
  | Ok _ -> true
  | Error _ -> false

let boolean_operators = [
  { arity = 2; name = "&&"; apply = apply_to_2_list (&&); };
]

let predicates : Parsetree.expression op list = [
  { arity = 2; name = "equal"; apply = apply_to_2_list equiv_ast; };
]

let replace variables = function
  | Variable v ->
    match Substitution.get_expr v variables with
    | Some e -> e
    | None -> raise (Undefined_var v)

let eval_op sub_fun pred exprs =
  let l = List.length exprs in
  if l <> pred.arity then
    raise (GuardEvaluationError ("The function " ^ pred.name ^ " is applied to the wrong number of arguments"))
  else
    pred.apply (List.map sub_fun exprs)

let rec eval variables = function
  | Bool_op (op, args) -> eval_op (eval variables) op args
  | Predicate (pred, exprs) -> eval_op (replace variables) pred exprs

let eval_union variables guards =
  List.fold_left (fun accu guard ->
      accu && eval variables guard
    )
    true
    guards
