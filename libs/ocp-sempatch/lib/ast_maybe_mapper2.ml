open Std_utils
open Parsetree
open Option.Infix

type 'a t = {
  expr : 'a t -> 'a -> expression -> expression -> (expression * 'a) option;
  pattern : 'a t -> 'a -> pattern -> pattern -> (pattern * 'a) option;
}

let merge_ast_and_env (env_merger : 'a -> 'a -> 'a) =
  Option.merge_inf (fun (accu_ast, accu_env) (ast, env) ->
      (fun new_env -> (ast :: accu_ast, new_env)) (env_merger accu_env env)
    )

let combine mapper merge default elements patch =
  let results = List.map2 mapper elements patch in
  List.fold_left (merge_ast_and_env merge) (Some ([], default)) results

let map_binding merge _default self defined_vars binding patch =
  self.pattern self defined_vars binding.pvb_pat patch.pvb_pat
  >>= (fun (pattern, pattern_env) ->
      self.expr self defined_vars binding.pvb_expr patch.pvb_expr
      >|= (fun (expr, expr_env) ->
          { binding with pvb_pat = pattern; pvb_expr = expr; }, merge pattern_env expr_env
        )
    )

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
  | Pexp_fun (lbl1, default1, pat1, expr1), Pexp_fun (_lbl2, _default2, pat2, expr2) ->
    (* TODO: handle labels and default values *)
    let mapped_arg = self.pattern self defined_vars pat1 pat2 in
    let mapped_expr = mapped_arg >>= (fun (_, env) -> self.expr self env expr1 expr2) in
    Option.merge_inf
      (fun (pat, env_pat) (expr, env_expr) -> Pexp_fun (lbl1, default1, pat, expr), merge env_pat env_expr)
      mapped_arg
      mapped_expr
  | Pexp_let (isrecl, bindingsl, exprl), Pexp_let (isrecr, bindingsr, exprr) when isrecl = isrecr ->
    let mapped_bindings = List.fold_left2 (fun accu bindingl bindingr ->
        accu
        >>= (fun (binding_list, env) ->
            map_binding merge default self defined_vars bindingl bindingr
            >|= (fun (parsed_binding, new_env) ->
                parsed_binding :: binding_list, merge env new_env)))
        (Some ([], defined_vars))
        bindingsl
        bindingsr
    in
    mapped_bindings >>= (fun (bindings, env) ->
        self.expr self env exprl exprr >|= (fun (mapped_expr, env) ->
            Pexp_let (isrecl, bindings, mapped_expr), env
          )
      )
  | Pexp_let _, _ | _, Pexp_let _
  | Pexp_apply _, _ | _, Pexp_apply _
  | Pexp_ident _, _ | _, Pexp_ident _
  | Pexp_constant _, _ | _, Pexp_constant _
    -> None
  | _ -> failwith "Non implemented"
  in Option.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc

let map_pattern _merge _default _self _defined_vars p patch =
  let maybe_desc =
    match p.ppat_desc, patch.ppat_desc with
    | Ppat_var _, _ -> None
    |_, _ -> failwith "Non implemented"
  in Option.map (fun (tree, env) -> { p with ppat_desc = tree; }, env) maybe_desc

let mk merge default = {
  expr = map_expr merge default;
  pattern = map_pattern merge default;
}

