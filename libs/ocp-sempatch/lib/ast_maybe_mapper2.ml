open Std_utils
open Parsetree

type 'a t = {
  expr : 'a t -> 'a -> expression -> expression -> (expression * 'a) option;
}

let merge_ast_and_env (env_merger : 'a -> 'a -> 'a) =
  Option.merge_inf (fun (accu_ast, accu_env) (ast, env) ->
      (fun new_env -> (ast :: accu_ast, new_env)) (env_merger accu_env env)
    )

let combine mapper merge default elements patch =
  let results = List.map2 mapper elements patch in
  List.fold_left (merge_ast_and_env merge) (Some ([], default)) results

let map_expr merge default self defined_vars e patch =
  let maybe_desc =
  match e.pexp_desc, patch.pexp_desc with
  | Pexp_ident _, Pexp_ident _
  | Pexp_constant _, Pexp_constant _ -> None
  | Pexp_tuple e1s, Pexp_tuple e2s -> Option.map (fun (trees, env) -> (Pexp_tuple trees, env)) @@ combine (self.expr self defined_vars) merge default e1s e2s
  | Pexp_apply (f1, [lbl1, arg1]), Pexp_apply (f2, [_lbl2, arg2]) ->
    Option.merge_inf
      (fun (f, env_f) (a, env_a) -> (Pexp_apply (f, [lbl1, a]), merge env_f env_a))
      (self.expr self defined_vars f1 f2)
      (self.expr self defined_vars arg1 arg2)
  | Pexp_apply _, _ | _, Pexp_apply _
  | Pexp_ident _, _ | _, Pexp_ident _
  | Pexp_constant _, _ | _, Pexp_constant _
    -> None
  | _ -> failwith "Non implemented"
  in Option.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc

let mk merge default = {
  expr = map_expr merge default;
}

