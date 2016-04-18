open Parsetree
open Std_utils
open Option.Infix

module StringMap = Map.Make(String)

let empty = StringMap.empty

let apply_replacements tree attributes var_replacements =
  (* TODO : Keep location from the original AST *)
  let new_tree = List.find_opt (fun x -> (fst x).Asttypes.txt = "__sempatch_replace") attributes
                 |> Option.map snd
                 |> Option.map (function
                     | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
                     | _ -> Parsed_patches.raisePatchError "Invalid replacement extension node"
                   )
                 |> Option.value tree
  in
  let mapper = Ast_mapper.(
      { default_mapper with
        expr = (fun _self e ->
            match e.pexp_desc with
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _} ->
              Variables.get_expr i var_replacements
              >|= (fun desc -> { e with pexp_desc = desc; })
              |> Option.value e
            | _ -> e
          );
      })
  in
  mapper.Ast_mapper.expr mapper new_tree

let apply patch expr =
  let is_meta_expr e = List.mem e Parsed_patches.(patch.header.meta_expr)
  and is_meta_binding b = List.mem b Parsed_patches.(patch.header.meta_bindings)
  and merge_envs = StringMap.merge (fun _ -> Misc.const) in
  let rec match_at_root =
    let open Ast_maybe_mapper2 in
    let default = mk (StringMap.merge (fun _ -> Misc.const)) StringMap.empty in
    {
      expr = (fun self defined_vars ({ pexp_desc = e1; _ } as expr1) ({ pexp_desc = e2; pexp_attributes = attrs2; _ } as expr2) ->
          let replacements =
            match e1, e2 with
            | Pexp_constant c1, Pexp_constant c2 when c1 = c2 -> Ok (expr1, defined_vars)
            | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Ok (expr1, defined_vars)
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _ }, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_binding j ->
              Error.ok_if (Variables.get_ident j defined_vars = (Some i)) (expr1, defined_vars) (expr1, defined_vars)
            | e, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_expr j ->
              (* TODO (one day...) treat the case where j is already defined as an expression *)
              Ok (expr1, Variables.add j (Variables.Expression e) empty)
            | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "__sempatch_inside" ->
              apply_to_expr defined_vars expr1 e
            | _ -> default.expr self defined_vars expr1 expr2
          in
          Error.map (fun (e, env) -> apply_replacements e attrs2 env, env) replacements
        );
      pattern = (fun _self _defined_vars pat1 pat2 ->
          let replacements =
            match pat1.ppat_desc, pat2.ppat_desc with
            | Ppat_var v, Ppat_var v' when v.Asttypes.txt = v'.Asttypes.txt -> Ok (pat1, empty)
            | Ppat_var { Asttypes.txt = v; _ }, Ppat_var { Asttypes.txt = v'; _ } when is_meta_binding v' ->
              Ok (pat1, StringMap.singleton v' (Variables.Ident v))
            | _ -> Error (pat1, empty)
          in replacements
        )
    }
  and apply_to_expr defined_vars expr patch =
    let open Ast_maybe_mapper2 in
    let open Res.Err_monad_infix in

    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ -> match_at_root.expr match_at_root defined_vars expr patch
    | Pexp_apply (fct, [lbl, arg]) ->
      apply_to_expr defined_vars fct patch
      >>= (fun (mapped_expr, env_expr) ->
          apply_to_expr defined_vars arg patch
          >>= (fun (mapped_arg, env_arg) ->
              let self_expr = { expr with pexp_desc = Pexp_apply (mapped_expr, [lbl, mapped_arg]);} in
              match_at_root.expr match_at_root defined_vars self_expr patch
              >|= (fun (mapped_self, env_self) ->
                  mapped_self, merge_envs env_expr (merge_envs env_arg env_self)
                )
            )
        )

    | Pexp_fun(lbl, default, pat, expr) ->
      let apply_some expr = apply_to_expr defined_vars expr patch
                            |> Res.map (fun (tree, env) -> Some tree, env) in
      Option.fold (fun _ -> apply_some) (Ok (None, empty)) default
      >>= (fun (mapped_default, env_default) ->
          apply_to_expr defined_vars expr patch
          >>= (fun (mapped_expr, env_expr) ->
              let self_expr = Ast_helper.Exp.mk (Pexp_fun (lbl, mapped_default, pat, mapped_expr)) in
              match_at_root.expr match_at_root defined_vars self_expr patch
              >|= (fun (mapped_self, env_self) ->
                  mapped_self, merge_envs env_default (merge_envs env_expr env_self)
                )
            )
        )
    | Pexp_let (isrec, bindings, expr) ->
      apply_to_bindings defined_vars patch bindings
      >>= (fun (mapped_bindings, env_bindings) ->
          apply_to_expr env_bindings expr patch
          >>= (fun (mapped_expr, env_combined) ->
              let self_expr = Ast_helper.Exp.mk (Pexp_let (isrec, mapped_bindings, mapped_expr)) in
              match_at_root.expr match_at_root defined_vars self_expr patch
              >|= (fun (mapped_self, env_self) ->
                  mapped_self, merge_envs env_combined env_self
                )
            )
        )

    | Pexp_tuple expr_list ->
      List.fold_left (fun mapped expr ->
          mapped >>= (fun (mapped_exprs, accu_env) ->
              apply_to_expr defined_vars expr patch
              >|= (fun (mapped_expr, env_expr) ->
                  mapped_expr :: mapped_exprs, merge_envs accu_env env_expr
                )
            )
        )
        (Ok ([], empty))
        expr_list
      >>= (fun (mapped_list, env_list) ->
          let self_expr = Ast_helper.Exp.mk (Pexp_tuple mapped_list) in
          match_at_root.expr match_at_root defined_vars self_expr patch
          >|= (fun (mapped_self, env_self) ->
              mapped_self, merge_envs env_list env_self
            )
        )
    | _ ->
      failwith "Not implemented yet"

  and apply_to_binding defined_vars patch binding =
    let open Res.Err_monad_infix in
    apply_to_expr defined_vars binding.pvb_expr patch
    >|= (fun (expr, env) -> { binding with pvb_expr = expr }, env)

  and apply_to_bindings defined_vars patch bindings =
    let open Res.Err_monad_infix in
    List.fold_left (fun mapped binding ->
        mapped >>= (fun (mapped_bindings, accu_env) ->
            apply_to_binding defined_vars patch binding
            >|= (fun (mapped_binding, new_env) ->
                mapped_binding :: mapped_bindings, merge_envs accu_env new_env
                )
          )
      )
      (Res.fail ([], empty))
      bindings

  in apply_to_expr empty expr Parsed_patches.(patch.body)
     |> Error.map fst
     |> Error.map_err fst
     |> (function Ok x -> x | Error x -> x)

