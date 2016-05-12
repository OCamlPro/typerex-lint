open Parsetree

module Type =
struct
  type header = {
    meta_expr : string list;
    message : string option;
    name : string;
    guard : Guard.t list;
  }

  type body = Parsetree.expression Automaton.t

  type patch = {
    header: header;
    body: body;
  }
end

open Type

type t = patch

type unprocessed_header = header
type unprocessed_body = Parsetree.expression
type unprocessed_patch = {
  unprocessed_header : unprocessed_header;
  unprocessed_body : unprocessed_body;
}

let void_header = {
  guard = [];
  meta_expr = [];
  message = None;
  name = "";
}

type setting =
  | Expressions of string list
  | Message of string
  | Name of string
  | Guard of Guard.t

let raisePatchError e = raise Failure.(SempatchException (Patch e))

let add_header_field header = function
  | Guard g -> { header with guard = g :: header.guard }
  | Expressions v -> { header with meta_expr = v @ header.meta_expr }
  | Message m -> { header with message = Some m }
  | Name m -> { header with name = m }

let header_from_list l = List.fold_left add_header_field void_header l

(** Checks wether l1 \subset l2
    where l1 and l2 represents unordonned sets of elements *)
let testInclusion l1 l2 =
  let mem_exn elt lst = if List.mem elt lst then () else failwith elt in
  try
    List.iter (fun x -> mem_exn x l2) l1
  with
    Failure x -> raisePatchError ("The meta-variable " ^ x
                                  ^ " is present in the substitution AST"
                                  ^ "and not in the original one")

let curryfying_mapper =
  let open Ast_mapper in
  { default_mapper with
    expr = (fun self e ->
        match e.pexp_desc with
        | Pexp_apply (f, args) ->
          let currified_applies = List.fold_left (fun acc (lbl, arg) ->
              {
                arg with
                pexp_desc = Pexp_apply (acc, [lbl, self.expr self arg]);
                pexp_attributes =
                  (Location.mknoloc "__sempatch_uncurryfy", PStr [])::[];
              }
            )
              (self.expr self f)
            args
          in { e with
               pexp_desc = currified_applies.pexp_desc;
               pexp_attributes = e.pexp_attributes;
             }
        | _ -> default_mapper.expr self e
      );
  }

let under_arg { pexp_desc = desc; _ } =
  match desc with
  | Pexp_apply (f, arg) -> Some (f, arg)
  | _ -> None

let uncurryfying_mapper =
  let open Ast_mapper in
  { default_mapper with
    expr = (fun self e ->
            match e.pexp_desc with
            | Pexp_apply (f, args)
              when List.exists
                  (fun (loc, _) -> loc.Location.txt = "__sempatch_uncurryfy")
                  f.pexp_attributes
              ->
              (
                match under_arg f with
                | Some (next_fun, next_arg) ->
                  self.expr self
                    { e with
                      pexp_desc = Pexp_apply (next_fun, next_arg @ args)
                    }
                | None -> default_mapper.expr self e
              )
            | _ -> default_mapper.expr self e
      );
  }

(** preprocess the patch before applying it

    Currently, this just means curryfiying the world
*)
let preprocess { unprocessed_header = header; unprocessed_body = body} =
  let open Ast_mapper in
  let meta_exprs_in_pre_patch  = ref []
  and metas_in_post_patch = ref []
  in
  let rec mkmapper in_replacement =
    { default_mapper
      with
        expr = (fun self e ->
            let new_expr =
              match e.pexp_desc with
              | Pexp_ident ({ Asttypes.txt = Longident.Lident v; _ })
                when List.mem v header.meta_expr ->
                let new_var = v in
                if
                  not in_replacement
                  && List.mem new_var !meta_exprs_in_pre_patch
                then
                  (* For the moment, don't allow meta expressions
                     to appear more than once in a patch *)
                  raisePatchError ("The variable " ^ v
                                   ^ " appears more than once in the patch")
                else
                  (
                    let processed_vars =
                      if in_replacement
                      then
                        metas_in_post_patch
                      else
                        meta_exprs_in_pre_patch
                    in
                    processed_vars := new_var :: !processed_vars;
                    e
                  )
              | _ -> e
            in default_mapper.expr self new_expr
          );
        pat = (fun self pat ->
            let new_pattern =
              match pat.ppat_desc with
              | Ppat_var { Asttypes.txt = i; _ }
                when List.mem i header.meta_expr ->
                if not in_replacement && List.mem i !meta_exprs_in_pre_patch
                then
                  raisePatchError ("The pattern " ^ i
                                   ^ " appears more than once in the patch")
                else (* TODO : Check if the variable appears out of scope ? *)
                  (
                    let processed_vars =
                      if in_replacement
                      then
                        metas_in_post_patch
                      else
                        meta_exprs_in_pre_patch
                    in
                    processed_vars := i :: !processed_vars;
                    pat
                  )
              | _ -> pat
            in default_mapper.pat self new_pattern
          );
        attribute = (fun self (name, payload) ->
            if name.Asttypes.txt = "__sempatch_replace" then
              let mapper= mkmapper true in
              name, mapper.payload mapper payload
            else default_mapper.attribute self (name, payload)
          )
    }
  in
  let map expr =
    let mapper = mkmapper false in
    curryfying_mapper.expr curryfying_mapper (mapper.expr mapper expr)
  in
  let processed_before_patch = map body
  in
  testInclusion
    !metas_in_post_patch
    (List.append !meta_exprs_in_pre_patch !meta_exprs_in_pre_patch);
  {
    header = { header with meta_expr = !meta_exprs_in_pre_patch; };
    body = Builder.from_expr !meta_exprs_in_pre_patch processed_before_patch;
  }

let preprocess_src_expr = curryfying_mapper.Ast_mapper.expr curryfying_mapper

let postprocess = uncurryfying_mapper.Ast_mapper.expr uncurryfying_mapper
