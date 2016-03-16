open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree


type t = Ast_mapper.mapper

(** Mapper composition *)
let (>>) patch1 patch2 =
  let (@!) f g x y = (f x) (g default_mapper y) in (* composition of mapper items -- TODO: Is this really what I wanna do ? *)
  {
  patch1 with (* TODO: write the rest *)
  expr = patch2.expr @! patch1.expr;
  pat = patch2.pat @! patch1.pat;
  structure_item = patch2.structure_item @! patch1.structure_item;
}

let txt_is loc = (=) loc.txt

let pattern_is_id pattern id =
  match pattern with
  | { ppat_desc = Ppat_var loc } when txt_is loc id -> true
  | _ -> false (* TODO: Is there another pattern to look at ? *)

let binds_id binder id =
  match binder with
  | { pvb_pat = pat } when pattern_is_id pat id -> true
  | _ -> false

(** {2 Patches definition} *)

let rename_var old_name new_name = {
  default_mapper with
  expr =
    fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_ident desc; }
        when txt_is desc (Longident.Lident old_name) ->
        { expr with
          pexp_desc = Pexp_ident { desc with txt = Longident.Lident new_name };
        }
      | p -> default_mapper.expr mapper p
}

let add_arg_fun fname arg_name =
  let matches_binding binding = binds_id binding fname
  in
  let transform_if_matches binding =
    if matches_binding binding then
      let pattern =
        Pat.mk ~loc:binding.pvb_loc (Ppat_var { txt = arg_name; loc = binding.pvb_loc })
      in
      { binding with
        pvb_expr =
          { binding.pvb_expr with
            pexp_desc = Pexp_fun ("", None, pattern, binding.pvb_expr);
          }
      }
    else
      binding
  in
  {
    default_mapper with
    expr =
      begin
        fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_let (isrec, bindings, e) } ->
            { expr with
              pexp_desc = Pexp_let (isrec, List.map transform_if_matches bindings, e)
            }
          | e -> default_mapper.expr mapper e
      end;
    structure_item =
      begin
        fun mapper item ->
          match item with
          | { pstr_desc = Pstr_value (isrec, bindings) } ->
            { item with
              pstr_desc = Pstr_value (isrec, List.map transform_if_matches bindings)
            }
          | i -> default_mapper.structure_item mapper i
      end;
  }

(* TODO: understand what I'm doing and remove the ugly "@guard" annotation *)
let make_fun_call var_name default_arg = {
  default_mapper with
  expr = fun mapper expr ->
    match expr with
    | { pexp_desc = Pexp_ident i; pexp_attributes = attrs } when txt_is i (Longident.Lident var_name) && not (List.exists (fun (x, _) -> txt_is x "@guard") attrs) ->
      let new_attrs = (Location.mkloc "@guard" expr.pexp_loc, PStr []) in
      { expr with
        pexp_desc = Pexp_apply (Exp.ident ~attrs:[new_attrs] (Location.mkloc (Longident.Lident var_name) expr.pexp_loc), [ "", default_arg ]);
      }
    | e -> default_mapper.expr mapper e
}

(** Just discard the argv argument *)
let to_ppx m = fun _ -> m

let () =
  let patch =
    default_mapper
    >> add_arg_fun "f" "x"
    >> rename_var ">>" ">>!"
    >> make_fun_call "z" (Exp.constant (Const_int 2))
    >> rename_var "z" "y"
  in register "patch" (to_ppx patch)
