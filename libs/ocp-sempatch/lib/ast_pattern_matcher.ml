open Parsetree
open Std_utils

module StringMap = Map.Make(String)

let is_meta : string list -> string -> bool = Fun.flip List.mem

let merge_envs_sup = Option.merge_sup (StringMap.merge (fun k -> Misc.const))
let merge_envs_inf = Option.merge_inf (StringMap.merge (fun k -> Misc.const))

let rec match_at_root meta_vars =
  let open Ast_traverser2 in
  let default = traverse merge_envs_inf None
  in
  from_mapper None {
    t2_expr = (fun self ({ pexp_desc = e1 } as expr1) ({ pexp_desc = e2 } as expr2) ->
        match e1, e2 with
        | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Some StringMap.empty
        | Pexp_ident i, Pexp_ident { Asttypes.txt = Longident.Lident j } when is_meta meta_vars j ->
          Some (StringMap.singleton j (Pexp_ident i))
        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, attrs); } ]) when loc.Asttypes.txt = "here" ->
          self.t2_expr self expr1 e
        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, attrs); } ]) when loc.Asttypes.txt = "inside" ->
          match_ast ~recurse:true meta_vars expr1 (Expr e)
        | _ -> default.t2_expr self expr1 expr2
      );
  }

and match_ast ?(recurse=true) meta_vars ast pattern =
  let open Asttypes in
  let open Ast_traverser in
  let single_traverser = apply_traverser2 merge_envs_sup None (match_at_root meta_vars) pattern
  in
  single_traverser.traverse_expr single_traverser ast
