open Parsetree
open Std_utils

module StringMap = Map.Make(String)

let is_meta : string list -> string -> bool = Fun.flip List.mem

let rec match_at_root meta_vars =
  let open Ast_traverser2 in
  let default = traverse (List.product (StringMap.merge (fun _ -> Misc.const))) []
  in
  from_mapper [] {
    t2_expr = (fun self ({ pexp_desc = e1; _ } as expr1) ({ pexp_desc = e2; _ } as expr2) ->
        match e1, e2 with
        | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> [StringMap.empty]
        | Pexp_ident i, Pexp_ident { Asttypes.txt = Longident.Lident j; _ } when is_meta meta_vars j ->
          [StringMap.singleton j (Pexp_ident i)]
        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "here" ->
          self.t2_expr self expr1 e
        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) when loc.Asttypes.txt = "inside" ->
          match_ast meta_vars expr1 (Expr e)
        | _ -> default.t2_expr self expr1 expr2
      );
  }

and match_ast meta_vars ast pattern =
  let open Ast_traverser in
  let single_traverser = apply_traverser2 List.append [] (match_at_root meta_vars) pattern
  in
  single_traverser.traverse_expr single_traverser ast
