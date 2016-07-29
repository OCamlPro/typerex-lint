open Parsetree
open Std_utils

type header = {
  meta_expr : string list;
  name : string;
  guard : Guard.t list;
  keyvals : string StringMap.t;
}
type body = Automaton.A.state
type patch = {
  header: header;
  body: body;
}

type unprocessed_header = header
type unprocessed_body = Parsetree.expression
type unprocessed_patch = {
  unprocessed_header : unprocessed_header;
  unprocessed_body : unprocessed_body;
}

type t = patch

let get_name p = p.header.name
let get_msg p = StringMap.get "message" p.header.keyvals
let get_metavariables p = p.header.meta_expr
let get_guard p = p.header.guard
let get_body p = p.body

let void_header = {
  guard = [];
  meta_expr = [];
  keyvals = StringMap.empty;
  name = "";
}

type setting =
  | Expressions of string list
  | KeyVal of string * string
  | Name of string
  | Guard of Guard.t

let raisePatchError e = raise Failure.(SempatchException (Patch e))

let add_header_field header = function
  | Guard g -> { header with guard = g :: header.guard }
  | Expressions v -> { header with meta_expr = v @ header.meta_expr }
  | KeyVal (k, v) -> { header with keyvals = StringMap.add k v header.keyvals }
  | Name m -> { header with name = m }

let header_from_list l = List.fold_left add_header_field void_header l

(** Checks wether l1 \subset l2
    where l1 and l2 represents unordonned sets of elements *)
let testInclusion l1 l2 =
  let mem_exn elt lst = if not (List.mem elt lst) then failwith elt in
  try
    List.iter (fun x -> mem_exn x l2) l1
  with
    Failure x -> raisePatchError ("The meta-variable " ^ x
                                  ^ " is present in the substitution AST"
                                  ^ "and not in the original one")

(** preprocess the patch before applying it *)
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
                  let processed_vars =
                    if in_replacement
                    then
                      metas_in_post_patch
                    else
                      meta_exprs_in_pre_patch
                  in
                  processed_vars := new_var :: !processed_vars;
                  if in_replacement then
                    e
                  else
                    {
                      e with
                      pexp_attributes =
                        (
                          Location.mkloc "__sempatch_metavar" e.pexp_loc,
                          PStr []
                        )
                        :: e.pexp_attributes
                    }
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
                  let processed_vars =
                    if in_replacement
                    then
                      metas_in_post_patch
                    else
                      meta_exprs_in_pre_patch
                  in
                  processed_vars := i :: !processed_vars;
                  if in_replacement then
                    pat
                  else
                    {
                      pat with
                      ppat_attributes =
                        (
                          Location.mkloc "__sempatch_metavar" pat.ppat_loc,
                          PStr []
                        )
                        :: pat.ppat_attributes
                    }
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
    mapper.expr mapper expr
  in
  let processed_before_patch = map body
  in
  testInclusion
    !metas_in_post_patch
    (List.append !meta_exprs_in_pre_patch !meta_exprs_in_pre_patch);
  {
    header = { header with meta_expr = !meta_exprs_in_pre_patch; };
    body =
      try
        Automaton.From.expression processed_before_patch
      with
        Failure.SempatchException (Failure.Non_implemented pos) ->
        raise Failure.(SempatchException (Non_implemented {
            pos with Location.loc_start = {
            pos.Location.loc_start
            with Lexing.pos_fname = "\"Patch " ^ header.name ^ "\"";
          }
          };
          ))
  }
