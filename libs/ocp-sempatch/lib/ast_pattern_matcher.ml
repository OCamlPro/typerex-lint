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
  let map processed_vars =
    let mapper = mkmapper processed_vars in
    mapper.expr mapper
  in
  let processed_before_patch = map metas_in_pre_patch body.before
  and processed_after_patch = map metas_in_post_patch body.after
  in
  testInclusion !metas_in_post_patch !metas_in_pre_patch;
  { name; header = { expr_variables = !metas_in_pre_patch }; body = { before = processed_before_patch; after = processed_after_patch }}

let is_meta = Fun.flip List.mem

let match_expression_desc meta_vars expr pattern =
  match expr, pattern with
  | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt -> Some StringMap.empty
  | Pexp_ident i, Pexp_ident { Asttypes.txt = Longident.Lident j } when is_meta meta_vars j -> Some (StringMap.singleton j (Pexp_ident i))
  | Pexp_ident _, Pexp_ident _ -> None
  | _ -> failwith "TODO"
