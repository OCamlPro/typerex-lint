open Std_utils
open Parsetree
open Res.Ok_monad_infix

type 'a t = {
  expr : 'a t -> 'a -> expression -> expression -> (expression * 'a, expression * 'a) Error.t;
  pattern : 'a t -> 'a -> pattern -> pattern -> (pattern * 'a, pattern * 'a) Error.t;
}

(* let merge_ast_and_env (env_merger : 'a -> 'a -> 'a) = *)
(*   Option.merge_inf (fun (accu_ast, accu_env) (ast, env) -> *)
(*       (fun new_env -> (ast :: accu_ast, new_env)) (env_merger accu_env env) *)
(*     ) *)
(*  *)
(* let combine mapper merge default elements patch = *)
(*   let results = List.map2 mapper elements patch in *)
(*   List.fold_left (merge_ast_and_env merge) (Some ([], default)) results *)
(*  *)
(* let map_binding merge _default self defined_vars binding patch = *)
(*   self.pattern self defined_vars binding.pvb_pat patch.pvb_pat *)
(*   >>= (fun (pattern, pattern_env) -> *)
(*       self.expr self defined_vars binding.pvb_expr patch.pvb_expr *)
(*       >|= (fun (expr, expr_env) -> *)
(*           { binding with pvb_pat = pattern; pvb_expr = expr; }, merge pattern_env expr_env *)
(*         ) *)
(*     ) *)

let map_binding merge _default self defined_vars binding patch =
  self.pattern self defined_vars binding.pvb_pat patch.pvb_pat
  >>= (fun (mapped_pattern, env_pattern) ->
      self.expr self defined_vars binding.pvb_expr patch.pvb_expr
      >|= (fun (mapped_expr, env_expr) ->
          { binding with pvb_pat = mapped_pattern; pvb_expr = mapped_expr; }, merge env_pattern env_expr
        )
    )

let map_bindings merge default self defined_vars =
  List.fold_left2 (fun accu binding patch_binding ->
      accu
      >>= (fun (bind_list, env) ->
          map_binding merge default self defined_vars binding patch_binding
          >|= (fun (mapped_binding, new_env) ->
              mapped_binding :: bind_list, merge env new_env
            )
        )
    )
    (Ok ([], default))


let map_expr merge default self defined_vars e patch =
  let maybe_desc =
  match e.pexp_desc, patch.pexp_desc with
  | Pexp_ident _, Pexp_ident _
  | Pexp_constant _, Pexp_constant _ -> Error (e.pexp_desc, default)
  | Pexp_tuple e1s, Pexp_tuple e2s -> (* Error.map (fun (trees, env) -> (Pexp_tuple trees, env)) @@ combine (self.expr self defined_vars) merge default e1s e2s *)
      List.fold_left2 (fun accu expr patch_expr ->
          accu >>= (fun (expr_list, accu_env) ->
              self.expr self defined_vars expr patch_expr
              >|= (fun (mapped_expr, new_env) ->
                  mapped_expr :: expr_list, merge accu_env new_env
                )
            )
      )
        (Ok ([], default))
        e1s
        e2s
      >|= (fun (exprs, env) ->
          Pexp_tuple exprs, env
        )
  | Pexp_apply (f1, [lbl1, arg1]), Pexp_apply (f2, [_lbl2, arg2]) ->
    self.expr self defined_vars f1 f2
    >>= (fun (mapped_f, env_f) -> 
        self.expr self defined_vars arg1 arg2
        >|= (fun (mapped_arg, env_arg) ->
            Pexp_apply (mapped_f, [lbl1, mapped_arg]), merge env_f env_arg
          )
      )
  (*   Option.merge_inf *)
  (*     (fun (f, env_f) (a, env_a) -> (Pexp_apply (f, [lbl1, a]), merge env_f env_a)) *)
  (*     (self.expr self defined_vars f1 f2) *)
  (*     (self.expr self defined_vars arg1 arg2) *)
  | Pexp_fun (lbl1, default1, pat1, expr1), Pexp_fun (_lbl2, _default2, pat2, expr2) ->
    (* TODO: handle labels and default values *)
    let mapped_arg = self.pattern self defined_vars pat1 pat2 in
    let mapped_expr = mapped_arg
      >>= (fun (_, env) -> self.expr self env expr1 expr2)
    in
    begin
      match mapped_arg, mapped_expr with
      | Ok (pat, env_pat), Ok (expr, env_expr) -> Ok (Pexp_fun (lbl1, default1, pat, expr), merge env_pat env_expr)
      | _, _ -> Error (e.pexp_desc, defined_vars)
    end
  | Pexp_let (isrecl, bindingsl, exprl), Pexp_let (isrecr, bindingsr, exprr) when isrecl = isrecr ->
    map_bindings merge default self defined_vars bindingsl bindingsr
    >>= (fun (mapped_bindings, env_bindings) ->
        self.expr self env_bindings exprl exprr
        >|= (fun (mapped_expr, env_expr) ->
            Pexp_let (isrecl, mapped_bindings, mapped_expr), env_expr
          )
      )
  (*   let mapped_bindings = List.fold_left2 (fun accu bindingl bindingr -> *)
  (*       accu *)
  (*       >>= (fun (binding_list, env) -> *)
  (*           map_binding merge default self defined_vars bindingl bindingr *)
  (*           >|= (fun (parsed_binding, new_env) -> *)
  (*               parsed_binding :: binding_list, merge env new_env))) *)
  (*       (Ok ([], defined_vars)) *)
  (*       bindingsl *)
  (*       bindingsr *)
  (*   in *)
  (*   mapped_bindings >>= (fun (bindings, env) -> *)
  (*       self.expr self env exprl exprr >|= (fun (mapped_expr, env) -> *)
  (*           Pexp_let (isrecl, bindings, mapped_expr), env *)
  (*         ) *)
  (*     ) *)
  | Pexp_let _, _ | _, Pexp_let _
  | Pexp_apply _, _ | _, Pexp_apply _
  | Pexp_ident _, _ | _, Pexp_ident _
  | Pexp_constant _, _ | _, Pexp_constant _
    -> Error (e.pexp_desc, default)
  | _ -> failwith "Non implemented"
  in Res.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc

let map_pattern _merge default _self _defined_vars p patch =
  let maybe_desc =
    match p.ppat_desc, patch.ppat_desc with
    | Ppat_var _, _ -> Error (p, default)
    |_, _ -> failwith "Non implemented"
  in Error.map (fun (tree, env) -> { p with ppat_desc = tree; }, env) maybe_desc

let mk merge default = {
  expr = map_expr merge default;
  pattern = map_pattern merge default;
}

