open Parsetree

type id = string

type header = {
  expr_variables : string list;
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
  let mkmapper processed_vars =
    { default_mapper
      with
        expr = (fun m e ->
            match e.pexp_desc with
            | Pexp_ident ({ Asttypes.txt = Longident.Lident v; _ } as ident) when List.mem v header.expr_variables ->
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
  (* and metas_in_post_patch = ref [] *)
  in
  let map processed_vars expr =
    let mapper = mkmapper processed_vars in
    curryfying_mapper.expr curryfying_mapper (mapper.expr mapper expr)
  in
  let processed_before_patch = map metas_in_pre_patch body
  (* and processed_after_patch = map metas_in_post_patch body.after *)
  in
  (* testInclusion !metas_in_post_patch !metas_in_pre_patch; *)
  { name; header = { expr_variables = !metas_in_pre_patch }; body = processed_before_patch; }

let preprocess_src_expr = curryfying_mapper.Ast_mapper.expr curryfying_mapper

