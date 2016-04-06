open Parsetree
open Std_utils

module StringMap = Map.Make(String)

exception PatchError of string

let raisePatchError e = raise (PatchError e)

(** Checks wether l1 \subset l2 where l1 and l2 represents unordonned sets of elements *)
let testInclusion l1 l2 =
  let mem_exn elt lst = if List.mem elt lst then () else failwith elt in
  try
    List.iter (fun x -> mem_exn x l2) l1
  with
    Failure x -> raisePatchError ("The meta-variable " ^ x ^ " is present in the substitution AST and not in the original one")

let curryfying_mapper =
  let open Ast_mapper in
  { default_mapper with
    expr = (fun self e ->
        match e.pexp_desc with
        | Pexp_apply (f, args) ->
          List.fold_left (fun acc (lbl, arg) ->
              {
                arg with
                pexp_desc = Pexp_apply (acc, [lbl,arg]);
              }
            )
            f
            args
        | _ -> default_mapper.expr self e
      );
  }

(** preprocess the patch before applying it

    Currently, this means adding a "?" in front of meta-variables to avoid
    conflict with real ones and perform sanity checks
*)
let preprocess { Parsed_patches.name; header; body} =
  let open Parsed_patches in
  let open Ast_mapper in
  let mkmapper processed_vars =
    { default_mapper
      with
        expr = (fun m e ->
            match e.pexp_desc with
            | Pexp_ident ({ Asttypes.txt = Longident.Lident v } as ident) when List.mem v header.expr_variables ->
              let new_var = "?" ^ v in
              if List.mem new_var !processed_vars then (* For the moment, don't allow meta variables to appear more than once in a patch *)
                raisePatchError ("The variable " ^ v ^ " appears more than once in the patch")
              else
                (
                  processed_vars := new_var :: !processed_vars;
                  { e with pexp_desc = Pexp_ident { ident with Asttypes.txt = Longident.Lident new_var } }
                )
            | _ -> default_mapper.expr m e
          )
    }
  and metas_in_pre_patch  = ref []
  and metas_in_post_patch = ref []
  in
  let map processed_vars expr =
    let mapper = mkmapper processed_vars in
    curryfying_mapper.expr curryfying_mapper (mapper.expr mapper expr)
  in
  let processed_before_patch = map metas_in_pre_patch body.before
  and processed_after_patch = map metas_in_post_patch body.after
  in
  testInclusion !metas_in_post_patch !metas_in_pre_patch;
  { name; header = { expr_variables = !metas_in_pre_patch }; body = { before = processed_before_patch; after = processed_after_patch }}

let preprocess_src_expr = curryfying_mapper.Ast_mapper.expr curryfying_mapper

let is_meta : string list -> string -> bool = Fun.flip List.mem

let merge_envs = Option.merge_sup (StringMap.merge (fun k -> Misc.const))

let rec match_at_root meta_vars =
  let open Ast_traverser2 in
  let default = traverse merge_envs None
  in
  from_mapper None {
    t2_expr = (fun self ({ pexp_desc = e1 } as expr1) ({ pexp_desc = e2 } as expr2) ->
        match e1, e2 with
        | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Some StringMap.empty
        | Pexp_ident i, Pexp_ident { Asttypes.txt = Longident.Lident j } when is_meta meta_vars j -> Some (StringMap.singleton j (Pexp_ident i))

        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, attrs); } ])
          when loc.Asttypes.txt = "here" -> self.t2_expr self expr1 e
        | _, Pexp_extension (loc, PStr [ { pstr_desc = Pstr_eval (e, attrs); } ])
          when loc.Asttypes.txt = "inside" -> match_expression meta_vars expr1 (Expr e)
        | _ -> default.t2_expr self expr1 expr2
      );
  }

and match_expression ?(recurse=true) meta_vars expr pattern =
  let open Asttypes in
  let single_traverser =
    let open Ast_traverser in
    let default = apply_traverser2 merge_envs None (match_at_root meta_vars) pattern
    in
    default
  in
  single_traverser.Ast_traverser.traverse_expr single_traverser expr
