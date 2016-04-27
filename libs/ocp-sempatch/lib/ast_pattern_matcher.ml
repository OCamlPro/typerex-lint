open Parsetree
open Std_utils

let apply_replacements tree attributes var_replacements =
  (* TODO : Keep location from the original AST *)
  let open Option.Infix in
  let new_tree = List.find_opt (fun x -> (fst x).Asttypes.txt = "__sempatch_replace") attributes
                 >|= snd
                 >|= (function
                     | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
                     | _ -> Parsed_patches.raisePatchError "Invalid replacement extension node"
                   )
                 |> Option.value tree
  in
  let mapper = Ast_mapper.(
      { default_mapper with
        expr = (fun self e ->
            match e.pexp_desc with
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _} ->
              Variables.get_expr i var_replacements
              >|= (fun desc -> { e with pexp_desc = desc; })
              |> Option.value e
            | _ -> default_mapper.expr self e
          );
      })
  in
  mapper.Ast_mapper.expr mapper new_tree

let apply patch expr =
  let is_meta_expr e = List.mem e Parsed_patches.(patch.header.meta_expr)
  and is_meta_binding b = List.mem b Parsed_patches.(patch.header.meta_bindings)
  and merge_envs = Variables.merge in
  let rec match_at_root =
    let open Ast_maybe_mapper2 in
    let default = mk Variables.merge in
    {
      expr = (fun self env ~patch:({ pexp_desc = e2; pexp_attributes = attrs2; _ } as expr2) ~expr:({ pexp_desc = e1; _ } as expr1) ->
          let replacements =
            match e1, e2 with
            | Pexp_constant c1, Pexp_constant c2 when c1 = c2 -> Ok (expr1, env)
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _ }, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_binding j ->
              Error.ok_if (Variables.get_ident j env = (Some i)) (expr1, env) (expr1, env)
            | e, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_expr j ->
              (* TODO (one day...) treat the case where j is already defined as an expression *)
              Ok (expr1, Variables.add_env j (Variables.Expression e) env)
            | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Ok (expr1, env)
            | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "__sempatch_inside" ->
              apply_to_expr env ~expr:expr1 ~patch:e
            | _ -> default.expr self env ~expr:expr1 ~patch:expr2
          in
          Error.map (fun (e, env) -> apply_replacements e attrs2 env, env) replacements
        );
      pattern = (fun self env ~patch:pat2 ~pat:pat1 ->
          let replacements =
            match pat1.ppat_desc, pat2.ppat_desc with
            | Ppat_var v, Ppat_var v' when v.Asttypes.txt = v'.Asttypes.txt -> Ok (pat1, env)
            | Ppat_var { Asttypes.txt = v; _ }, Ppat_var { Asttypes.txt = v'; _ } when is_meta_binding v' ->
              Ok (pat1, Variables.add_env v' (Variables.Ident v) env)
            | _ -> default.pattern self env ~patch:pat2 ~pat:pat1
          in replacements
        )
    }
  and apply_to_expr env ~patch ~expr =
    let open Ast_maybe_mapper2 in
    let open Res.Err_monad_infix in

    let desc_err =
      match expr.pexp_desc with
      | Pexp_ident _ | Pexp_constant _ as d -> Error.fail (d, [env])
      | Pexp_apply (fct, [lbl, arg]) ->
        apply_to_expr env ~expr:fct ~patch
        >>= (fun (mapped_expr, env_expr) ->
            apply_to_expr env ~expr:arg ~patch
            >|= (fun (mapped_arg, env_arg) ->
                Pexp_apply (mapped_expr, [lbl, mapped_arg]), [env_expr; env_arg]
              )
          )
      | Pexp_fun(lbl, default, pat, expr) ->
        let apply_some expr = apply_to_expr env ~expr ~patch
                              |> Res.map (fun (tree, env) -> Some tree, env) in
        Option.fold (fun _ -> apply_some) (Ok (None, env)) default
        >>= (fun (mapped_default, env_default) ->
            apply_to_expr env ~expr ~patch
            >|= (fun (mapped_expr, env_expr) ->
                Pexp_fun (lbl, mapped_default, pat, mapped_expr), [ env_expr; env_default]
              )
          )
      | Pexp_let (isrec, bindings, expr) ->
        apply_to_bindings env patch bindings
        >>= (fun (mapped_bindings, env_bindings) ->
            apply_to_expr env_bindings ~expr ~patch
            >|= (fun (mapped_expr, env_combined) ->
                Pexp_let (isrec, mapped_bindings, mapped_expr), [ env_combined ]
              )
          )

      | Pexp_tuple expr_list ->
        List.fold_left (fun mapped expr ->
            mapped >>= (fun (mapped_exprs, accu_env) ->
                apply_to_expr env ~expr ~patch
                >|= (fun (mapped_expr, env_expr) ->
                    mapped_expr :: mapped_exprs, merge_envs accu_env env_expr
                  )
              )
          )
          (Ok ([], env))
          expr_list
        >|= (fun (mapped_list, env_list) ->
            Pexp_tuple mapped_list, [ env_list ]
          )

      | Pexp_ifthenelse (cif, cthen, celse) ->
        apply_to_expr env ~expr:cif ~patch
        >>= (fun (mapped_cif, env_cif) ->
            apply_to_expr env ~expr:cthen ~patch
            >>= (fun (mapped_cthen, env_cthen) ->
                apply_to_maybe_expr env patch celse
                >|= (fun (mapped_celse, env_celse) ->
                    Pexp_ifthenelse (mapped_cif, mapped_cthen, mapped_celse), [ env_cif; env_cthen; env_celse ]
                  )
              )
          )
      | Pexp_function cases ->
        apply_to_cases env patch cases
        >|= (fun (mapped_cases, env_cases) ->
            Pexp_function mapped_cases, [ env_cases ]
          )

      | Pexp_construct (ident, expr) ->
        apply_to_maybe_expr env patch expr
        >|= (fun (mapped_expr, env_expr) ->
            Pexp_construct (ident, mapped_expr), [env_expr]
          )

      | Pexp_match (expr, cases) ->
        apply_to_cases env patch cases
        >>= (fun (mapped_cases, env_cases) ->
            apply_to_expr env_cases ~expr ~patch
            >|= (fun (mapped_expr, env_expr) ->
                Pexp_match (mapped_expr, mapped_cases),
                [ env_cases; env_expr ]
              )
          )

      | _ ->
        Pprintast.expression Format.std_formatter expr;
        Format.print_newline ();
        failwith "Not implemented yet"
    in desc_err
    >>= (fun (mapped_desc, env_exprs) ->
        let self_expr = { expr with pexp_desc = mapped_desc } in
        match_at_root.expr match_at_root env ~expr:self_expr ~patch
        >|= (fun (mapped_self, env_self) ->
            mapped_self, (List.fold_left merge_envs env_self env_exprs)
          )

      )

  and apply_to_case env patch { pc_lhs; pc_guard; pc_rhs } =
    let open Res.Err_monad_infix in
    apply_to_maybe_expr env patch pc_guard
    >>= (fun (mapped_guard, env_guard) ->
        apply_to_expr env ~expr:pc_rhs ~patch
        >|= (fun (mapped_rhs, env_rhs) ->
            {
              pc_lhs;
              pc_guard = mapped_guard;
              pc_rhs = mapped_rhs;
            },
            merge_envs env_guard env_rhs
          )
      )

  and apply_to_list mapper env patch =
    let open Res.Err_monad_infix in
    List.fold_left (fun mapped elt ->
        mapped >>= (fun (mapped_elts, accu_env) ->
            mapper env patch elt
            >|= (fun (mapped_elt, new_env) ->
                mapped_elt :: mapped_elts, merge_envs accu_env new_env
              )
          )
      )
      (Res.fail ([], env))

  and apply_to_cases env patch = apply_to_list apply_to_case env patch

  and apply_to_binding env patch binding =
    let open Res.Err_monad_infix in
    apply_to_expr env ~expr:binding.pvb_expr ~patch
    >|= (fun (expr, env) -> { binding with pvb_expr = expr }, env)

  and apply_to_bindings env patch bindings =
    let open Res.Err_monad_infix in
    List.fold_left (fun mapped binding ->
        mapped >>= (fun (mapped_bindings, accu_env) ->
            apply_to_binding env patch binding
            >|= (fun (mapped_binding, new_env) ->
                mapped_binding :: mapped_bindings, merge_envs accu_env new_env
                )
          )
      )
      (Res.fail ([], env))
      bindings

  and apply_to_maybe_expr env patch =
    let open Res.Err_monad_infix in
    function
    | Some expr -> apply_to_expr env ~expr ~patch
      >|= (fun (expr, env) -> Some expr, env)
    | None -> Ok (None, env)

  in
  let expr = Parsed_patches.preprocess_src_expr expr
  and patch = Parsed_patches.preprocess patch
  in
  apply_to_expr Variables.empty ~expr ~patch:Parsed_patches.(patch.body)
  |> Res.map (fun (tree, env) -> Parsed_patches.postprocess tree, env)
     (* |> Error.map fst *)
     (* |> Error.map_err fst *)
     (* |> (function Ok x -> x | Error x -> x) *)

