open Parsetree

type id = string

type header = {
  meta_expr : string list;
  meta_bindings : string list;
}

type body = Parsetree.expression

type t = {
  name: id;
  header: header;
  body: body;
}

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
          let currified_applies = List.fold_left (fun acc (lbl, arg) ->
              {
                arg with
                pexp_desc = Pexp_apply (acc, [lbl,arg]);
              }
            )
            f
            args
          in { e with pexp_desc = currified_applies.pexp_desc }
        | _ -> default_mapper.expr self e
      );
  }

(** preprocess the patch before applying it

    Currently, this just means curryfiying the world
*)
let preprocess { name; header; body} =
  let open Ast_mapper in
  let meta_exprs_in_pre_patch  = ref []
  and meta_bindings_in_pre_patch = ref []
  and metas_in_post_patch = ref []
  in
  let mkmapper in_replacement =
    { default_mapper
      with
        expr = (fun self e ->
            let new_expr =
              match e.pexp_desc with
              | Pexp_ident ({ Asttypes.txt = Longident.Lident v; _ }) when List.mem v header.meta_expr ->
                let new_var = v in
                if not in_replacement && List.mem new_var !meta_exprs_in_pre_patch then
                  (* For the moment, don't allow meta expressions to appear more than once in a patch *)
                  raisePatchError ("The variable " ^ v ^ " appears more than once in the patch")
                else
                  (
                    let processed_vars = if in_replacement then metas_in_post_patch else meta_exprs_in_pre_patch in
                    processed_vars := new_var :: !processed_vars;
                    e
                  )
              | _ -> e
            in default_mapper.expr self new_expr
          );
        pat = (fun self pat ->
            let new_pattern =
              match pat.ppat_desc with
              | Ppat_var { Asttypes.txt = i; _ } when List.mem i header.meta_expr || List.mem i header.meta_bindings ->
                if not in_replacement && List.mem i !meta_bindings_in_pre_patch then
                  raisePatchError ("The pattern " ^ i ^ " appears more than once in the patch")
                else (* TODO : Check if the variable appears out of scope ? *)
                  (
                    let processed_vars = if in_replacement then metas_in_post_patch else meta_bindings_in_pre_patch in
                    processed_vars := i :: !processed_vars;
                    pat
                  )
              | _ -> pat
            in default_mapper.pat self new_pattern
          );
    }
  in
  let map expr =
    let mapper = mkmapper false in
    curryfying_mapper.expr curryfying_mapper (mapper.expr mapper expr)
  in
  let processed_before_patch = map body
  in
  testInclusion !metas_in_post_patch (List.append !meta_bindings_in_pre_patch !meta_exprs_in_pre_patch);
  { name; header = { meta_expr = !meta_exprs_in_pre_patch; meta_bindings = !meta_bindings_in_pre_patch; }; body = processed_before_patch; }

let preprocess_src_expr = curryfying_mapper.Ast_mapper.expr curryfying_mapper
