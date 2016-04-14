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
    let mkapply lbl (expr_f, env_f) (expr_arg, env_arg) = Pexp_apply (expr_f, [lbl, expr_arg]), StringMap.merge (fun _ -> Misc.const) env_f env_arg in
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ -> match_at_root.expr match_at_root defined_vars expr pattern
    | Pexp_apply (fct, [lbl, arg]) ->
      let under_f = apply_to_expr defined_vars pattern fct
      and under_arg = apply_to_expr defined_vars pattern arg
      in
      let merged = mkapply lbl (under_f |? (fct, StringMap.empty)) (under_arg |? (arg, StringMap.empty)) in
      let res = match_at_root.expr match_at_root defined_vars { expr with pexp_desc = (fst merged) } pattern
      in
      Option.(
        some_if (is_some under_f || is_some under_arg || is_some res)
          (
            let res_expr, env = res |? ({ expr with pexp_desc = fst merged }, snd merged) in
            res_expr, StringMap.merge (fun _ -> Misc.const) env (snd merged)
          )
      )
    | Pexp_fun(lbl, default, pat, expr) ->
      let under_pat_opt = Some (pat, empty) in
      let under_expr_opt = apply_to_expr defined_vars pattern expr
      in
      let under_pat = under_pat_opt |? (pat, empty)
      and under_expr = under_expr_opt |? (expr, empty)
      in
      let merged =
        Pexp_fun (
          lbl,
          default,
          fst under_pat,
          fst under_expr
        ), StringMap.merge (fun _ -> Misc.const) (snd under_pat) (snd under_expr)
      in
      let res = match_at_root.expr match_at_root defined_vars { expr with pexp_desc = fst merged} pattern
      in
      Option.(
        some_if (is_some under_pat_opt || is_some under_expr_opt || is_some res )
          (
            let res_expr, env = res |? ({ expr with pexp_desc = fst merged }, snd merged) in
            res_expr, StringMap.merge (fun _ -> Misc.const) env (snd merged)
          )
      )

    | _ -> failwith "Not implemented yet"
  in apply_to_expr empty Parsed_patches.(patch.body) expr
     |> Option.map fst
     |> Option.value expr
