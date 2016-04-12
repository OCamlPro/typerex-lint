open Parsetree
open Std_utils

module StringMap = Map.Make(String)

let is_meta : string list -> string -> bool = Fun.flip List.mem

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
              (
                try
                  { e with pexp_desc = StringMap.find i var_replacements }
                with
                  Not_found -> e
              )
            | _ -> e
          );
      })
  in
  mapper.Ast_mapper.expr mapper new_tree

let apply patch expr =
  let is_meta = is_meta Parsed_patches.(patch.header.expr_variables) in
  let rec match_at_root =
    let open Ast_maybe_mapper2 in
    let default = mk (StringMap.merge (fun _ -> Misc.const)) StringMap.empty in
    {
      expr = (fun self ({ pexp_desc = e1; _ } as expr1) ({ pexp_desc = e2; pexp_attributes = attrs2; _ } as expr2) ->
          let replacements =
            match e1, e2 with
            | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Some (expr1, StringMap.empty)
            | e, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta j ->
              Some (expr1, StringMap.singleton j e)
            | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "inside" ->
              apply_to_expr e expr1
            | _ -> default.expr self expr1 expr2
          in
          Option.map (fun (e, env) -> apply_replacements e attrs2 env, env) replacements
        );
    }
  and apply_to_expr pattern expr =
    let open Ast_maybe_mapper2 in
    let open Option.Infix in
    let mkapply lbl (expr_f, env_f) (expr_arg, env_arg) = Pexp_apply (expr_f, [lbl, expr_arg]), StringMap.merge (fun _ -> Misc.const) env_f env_arg in
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ -> match_at_root.expr match_at_root expr pattern
    | Pexp_apply (fct, [lbl, arg]) ->
      let under_f = apply_to_expr pattern fct
      and under_arg = apply_to_expr pattern arg
      in let merged = mkapply lbl (under_f |? (fct, StringMap.empty)) (under_arg |? (arg, StringMap.empty));
      in
      let res = match_at_root.expr match_at_root { expr with pexp_desc = (fst merged) } pattern
      in
      Option.(
        some_if (is_some under_f || is_some under_arg || is_some res)
          (
            let res_expr, env = res |? ({ expr with pexp_desc = fst merged }, snd merged) in
            res_expr, StringMap.merge (fun _ -> Misc.const) env (snd merged)
          )
      )
    | _ -> failwith "Not implemented yet"
  in apply_to_expr Parsed_patches.(patch.body) expr
     |> Option.map fst
     |> Option.value expr
