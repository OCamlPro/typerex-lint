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
  and is_meta_binding b = List.mem b Parsed_patches.(patch.header.meta_bindings) in
  let rec match_at_root =
    let open Ast_maybe_mapper2 in
    let default = mk (StringMap.merge (fun _ -> Misc.const)) StringMap.empty in
    {
      expr = (fun self defined_vars ({ pexp_desc = e1; _ } as expr1) ({ pexp_desc = e2; pexp_attributes = attrs2; _ } as expr2) ->
          let replacements =
            match e1, e2 with
            | Pexp_constant c1, Pexp_constant c2 when c1 = c2 -> Some (expr1, defined_vars)
            | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Some (expr1, defined_vars)
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _ }, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_binding j ->
              Option.some_if (Variables.get_ident j defined_vars = (Some i)) (expr1, defined_vars)
            | e, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta_expr j ->
              (* TODO (one day...) treat the case where j is already defined as an expression *)
              (* Option.some_if (not (Variables.is_defined_ident j defined_vars)) *)
              Some (expr1, Variables.add j (Variables.Expression e) empty)
            | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "__sempatch_inside" ->
              apply_to_expr defined_vars e expr1
            | _ -> default.expr self defined_vars expr1 expr2
          in
          Option.map (fun (e, env) -> apply_replacements e attrs2 env, env) replacements
        );
      pattern = (fun _self _defined_vars pat1 pat2 ->
          let replacements =
            match pat1.ppat_desc, pat2.ppat_desc with
            | Ppat_var v, Ppat_var v' when v.Asttypes.txt = v'.Asttypes.txt -> Some (pat1, empty)
            | Ppat_var { Asttypes.txt = v; _ }, Ppat_var { Asttypes.txt = v'; _ } when is_meta_binding v' ->
              Some (pat1, StringMap.singleton v' (Variables.Ident v))
            | _ -> None
          in replacements
        )
    }
  and apply_to_expr defined_vars pattern expr =
    let open Ast_maybe_mapper2 in
    let open Option.Infix in
    let merge_two_exprs merge_fun e1 e2 =
      let under_e1 = apply_to_expr defined_vars pattern e1
      and under_e2 = apply_to_expr defined_vars pattern e2 in
      let merged = merge_fun (under_e1 |? (e1, empty)) (under_e2 |? (e2, empty)) in
      let default_res = { expr with pexp_desc = fst merged } in
      let res = match_at_root.expr match_at_root defined_vars default_res pattern in
      Option.some_if Option.(is_some under_e1 || is_some under_e2 || is_some res) (
        let res_expr, env = res |? (default_res, snd merged) in
        res_expr, StringMap.merge (fun _ -> Misc.const) env (snd merged)
      )
    and merge_one_expr merge_fun e =
      let under = apply_to_expr defined_vars pattern e in
      let merged = merge_fun (under |? (e, empty)) in
      let default_res = { expr with pexp_desc = fst merged } in
      let res = match_at_root.expr match_at_root defined_vars default_res pattern in
      Option.some_if Option.(is_some under || is_some res) (
        let res_expr, env  = res |? (default_res, snd merged) in
        res_expr, StringMap.merge (fun _ -> Misc.const) env (snd merged)
      )
    in
    let mkapply lbl (expr_f, env_f) (expr_arg, env_arg) = Pexp_apply (expr_f, [lbl, expr_arg]), StringMap.merge (fun _ -> Misc.const) env_f env_arg in
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ -> match_at_root.expr match_at_root defined_vars expr pattern
    | Pexp_apply (fct, [lbl, arg]) ->
      merge_two_exprs (mkapply lbl) fct arg
    | Pexp_fun(lbl, default, pat, expr) ->
      Option.fold
        (fun _ value -> merge_two_exprs
            (fun (default_expr, default_env) (body_expr, body_env) ->
               Pexp_fun (lbl, Some default_expr, pat, body_expr), StringMap.merge (fun _ -> Misc.const) default_env body_env)
            value expr
        )
        (merge_one_expr (fun (expr, env) -> Pexp_fun (lbl, default, pat, expr), env) expr)
        default
    | _ -> failwith "Not implemented yet"
  in apply_to_expr empty Parsed_patches.(patch.body) expr
     |> Option.map fst
     |> Option.value expr
