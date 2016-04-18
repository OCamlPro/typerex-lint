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

    (* let merge_two merge_fun x1 default1 x2 default2 = *)
    (*   let merged = merge_fun (x1 |? default1) (x2 |? default2) in *)
    (*   let res = match_at_root.expr match_at_root defined_vars (fst merged) patch in *)
    (*   Option.some_if Option.(is_some x1 || is_some x2 || is_some res) ( *)
    (*     let res_expr, res_env = res |? merged in *)
    (*     res_expr, StringMap.merge (fun _ -> Misc.const) res_env (snd merged) *)
    (*   ) *)
    (* in *)
    (*  *)
    (* let merge_two_exprs merge_fun e1 e2 = *)
    (*   let under_e1 = apply_to_expr defined_vars e1 patch *)
    (*   and under_e2 = apply_to_expr defined_vars e2 patch in *)
    (*   merge_two *)
    (*     (fun x1 x2 -> let (e, env) = merge_fun x1 x2 in { expr with pexp_desc = e }, env) *)
    (*     under_e1 (e1, empty) *)
    (*     under_e2 (e2, empty) *)
    (* and merge_one_expr merge_fun e = *)
    (*   let under = apply_to_expr defined_vars e patch in *)
    (*   merge_two (fun x1 _ -> let (e, env) = merge_fun x1 in { expr with pexp_desc = e }, env) *)
    (*     under (e, empty) *)
    (*     None () *)
    (* in *)
    (* let mkapply lbl (expr_f, env_f) (expr_arg, env_arg) = Pexp_apply (expr_f, [lbl, expr_arg]), StringMap.merge (fun _ -> Misc.const) env_f env_arg in *)
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ -> match_at_root.expr match_at_root defined_vars expr patch
    (* | Pexp_apply (fct, [lbl, arg]) -> *)
    (*   merge_two_exprs (mkapply lbl) fct arg *)
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
    (* | Pexp_let (isrec, bindings, expr) -> *)
    (*   let new_bindings_opt = apply_to_bindings defined_vars patch bindings in *)
    (*   let under_expr = apply_to_expr (snd (new_bindings_opt |? (bindings, empty))) expr patch in *)
    (*   merge_two (fun (bind, bind_env) (expr, expr_env) -> { expr with pexp_desc = Pexp_let (isrec, bind, expr) }, StringMap.merge (fun _ -> Misc.const) bind_env expr_env) *)
    (*     new_bindings_opt (bindings, empty) *)
    (*     under_expr (expr, empty) *)
    | _ -> failwith "Not implemented yet"

  (* and apply_to_binding defined_vars patch binding = *)
  (*   let open Error.Err_monad_infix in *)
  (*   apply_to_expr defined_vars binding.pvb_expr patch *)
  (*   >|= (fun (expr, env) -> { binding with pvb_expr = expr }, env) *)
  (*  *)
  (* and apply_to_bindings defined_vars patch bindings = *)
  (*   let one_true = ref false in *)
  (*   let res =List.fold_left *)
  (*       (fun (mapped_bindings, accu_env) binding -> *)
  (*          let mapped_binding_opt = apply_to_binding defined_vars patch binding in *)
  (*          Option.iter (fun _ -> one_true := true) mapped_binding_opt; *)
  (*          let (mapped_binding, env) = mapped_binding_opt |? (binding, empty) in *)
  (*          (mapped_binding :: mapped_bindings, StringMap.merge (fun _ -> Misc.const) accu_env env) *)
  (*       ) *)
  (*       ([], defined_vars) *)
  (*       bindings *)
  (*   in Option.some_if !one_true res *)

  in apply_to_expr empty expr Parsed_patches.(patch.body)
     |> Error.map fst
     |> Error.map_err fst
     |> (function Ok x -> x | Error x -> x)

