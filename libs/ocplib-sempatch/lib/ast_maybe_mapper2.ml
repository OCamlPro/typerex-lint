open Std_utils
open Parsetree
open Res.Ok_monad_infix

type 'a t = {
  expr : 'a t -> 'a -> patch:expression -> expr:expression -> (expression * 'a, expression * 'a) Error.t;
  pattern : 'a t -> 'a -> patch:pattern -> pat:pattern -> (pattern * 'a, pattern * 'a) Error.t;
}

let map_binding merge self env binding patch =
  self.pattern self env ~pat:binding.pvb_pat ~patch:patch.pvb_pat
  >>= (fun (mapped_pattern, env_pattern) ->
      self.expr self env ~expr:binding.pvb_expr ~patch:patch.pvb_expr
      >|= (fun (mapped_expr, env_expr) ->
          { binding with pvb_pat = mapped_pattern; pvb_expr = mapped_expr; }, merge env_pattern env_expr
        )
    )

let map_bindings merge self env =
  List.fold_left2 (fun accu binding patch_binding ->
      accu
      >>= (fun (bind_list, env) ->
          map_binding merge self env binding patch_binding
          >|= (fun (mapped_binding, new_env) ->
              mapped_binding :: bind_list, merge env new_env
            )
        )
    )
    (Ok ([], env))

let map_maybe_expr _merge self env expr_opt patch_opt =
  match expr_opt, patch_opt with
  | Some expr, Some patch -> self.expr self env ~expr ~patch >|= (fun (mapped, env) -> Some mapped, env)
  | None, None -> Ok (None, env)
  | _ -> Error (expr_opt, env)

let map_case merge self env
    { pc_lhs = lhs_e; pc_guard = guard_e; pc_rhs = rhs_e }
    { pc_lhs = lhs_patch; pc_guard = guard_patch; pc_rhs = rhs_patch }
  =
  self.pattern self env ~patch:lhs_patch ~pat:lhs_e
  >>= (fun (mapped_lhs, env_lhs) ->
      map_maybe_expr merge self env_lhs guard_e guard_patch
      >>= (fun (mapped_guard, env_guard) ->
          self.expr self env_lhs ~expr:rhs_e ~patch:rhs_patch
          >|= (fun (mapped_rhs, env_rhs) ->
              {
                pc_lhs = mapped_lhs;
                pc_guard = mapped_guard;
                pc_rhs = mapped_rhs;
              },
              merge env_lhs (merge env_guard env_rhs)
            )
        )
    )

let map_cases merge self env =
  List.fold_left2 (fun accu binding patch_binding ->
      accu
      >>= (fun (bind_list, env) ->
          map_case merge self env binding patch_binding
          >|= (fun (mapped_binding, new_env) ->
              mapped_binding :: bind_list, merge env new_env
            )
        )
    )
    (Ok ([], env))

let map_expr merge self env ~patch ~expr =
  let e = expr in
  let maybe_desc =
  match e.pexp_desc, patch.pexp_desc with
  | Pexp_ident _, Pexp_ident _
  | Pexp_constant _, Pexp_constant _ -> Error (e.pexp_desc, env)
  | Pexp_tuple e1s, Pexp_tuple e2s ->
      List.fold_left2 (fun accu expr patch_expr ->
          accu >>= (fun (expr_list, accu_env) ->
              self.expr self env ~expr ~patch:patch_expr
              >|= (fun (mapped_expr, new_env) ->
                  mapped_expr :: expr_list, merge accu_env new_env
                )
            )
      )
        (Ok ([], env))
        e1s
        e2s
      >|= (fun (exprs, env) ->
          Pexp_tuple exprs, env
        )

  | Pexp_construct (identl, exprl), Pexp_construct (identr, exprr) when identl.Asttypes.txt = identr.Asttypes.txt ->
    map_maybe_expr merge self env exprl exprr
    >|= (fun (mapped_expr, env_expr) ->
        Pexp_construct (identl, mapped_expr), env_expr
      )

  | Pexp_apply (f1, [lbl1, arg1]), Pexp_apply (f2, [_lbl2, arg2]) ->
    self.expr self env ~expr:f1 ~patch:f2
    >>= (fun (mapped_f, env_f) ->
        self.expr self env ~expr:arg1 ~patch:arg2
        >|= (fun (mapped_arg, env_arg) ->
            Pexp_apply (mapped_f, [lbl1, mapped_arg]), merge env_f env_arg
          )
      )
  | Pexp_fun (lbl1, default1, pat1, expr1), Pexp_fun (_lbl2, _default2, pat2, expr2) ->
    (* TODO: handle labels and default values *)
    let mapped_arg = self.pattern self env ~pat:pat1 ~patch:pat2 in
    let mapped_expr = mapped_arg
      >>= (fun (_, env) -> self.expr self env ~expr:expr1 ~patch:expr2)
    in
    begin
      match mapped_arg, mapped_expr with
      | Ok (pat, env_pat), Ok (expr, env_expr) -> Ok (Pexp_fun (lbl1, default1, pat, expr), merge env_pat env_expr)
      | _, _ -> Error (e.pexp_desc, env)
    end
  | Pexp_let (isrecl, bindingsl, exprl), Pexp_let (isrecr, bindingsr, exprr) when isrecl = isrecr ->
    map_bindings merge self env bindingsl bindingsr
    >>= (fun (mapped_bindings, env_bindings) ->
        self.expr self env_bindings ~expr:exprl ~patch:exprr
        >|= (fun (mapped_expr, env_expr) ->
            Pexp_let (isrecl, mapped_bindings, mapped_expr), env_expr
          )
      )

  | Pexp_ifthenelse (ifl, thenl, elsel), Pexp_ifthenelse(ifr, thenr, elser) ->
    self.expr self env ~expr:ifl ~patch:ifr
    >>= (fun (mapped_if, env_if) ->
        self.expr self env ~expr:thenl ~patch:thenr
        >>= (fun (mapped_then, env_then) ->
            map_maybe_expr merge self env elsel elser
            >|= (fun (mapped_else, env_else) ->
                Pexp_ifthenelse (mapped_if, mapped_then, mapped_else), merge env_if (merge env_then env_else)
            )
          )
      )

  | Pexp_function casesl, Pexp_function casesr ->
    map_cases merge self env casesl casesr
    >|= (fun (mapped_cases, env_cases) ->
        Pexp_function mapped_cases, env_cases
      )

  | Pexp_let _, _ | _, Pexp_let _
  | Pexp_apply _, _ | _, Pexp_apply _
  | Pexp_ident _, _ | _, Pexp_ident _
  | Pexp_constant _, _ | _, Pexp_constant _
  | Pexp_construct _, _ | _, Pexp_construct _
  | Pexp_ifthenelse _, _| _, Pexp_ifthenelse _
    -> Error (e.pexp_desc, env)
  | _ -> failwith "Non implemented"
  in
  match Res.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc with
  | Ok (expr, attrs) -> Ok (expr, Variables.set_loc [attrs.Variables.env, e.pexp_loc] attrs)
  | Error (expr, attrs) -> Error (expr, Variables.set_loc [] attrs)
(* |> Error.map (fun (expr, attrs) -> expr, Variables.set_loc [attrs.Variables.env, e.pexp_loc] attrs) *)

let map_pattern _merge _self env ~patch ~pat =
  let maybe_desc =
    match pat.ppat_desc, patch.ppat_desc with
    | Ppat_var _, _ -> Error (pat, env)
    |_, _ -> failwith "Non implemented"
  in Error.map (fun (tree, env) -> { pat with ppat_desc = tree; }, env) maybe_desc

let mk merge = {
  expr = map_expr merge;
  pattern = map_pattern merge;
}
