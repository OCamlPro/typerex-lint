open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree


type t = Ast_mapper.mapper

let apply_and_compose f g x y = (f x) (g default_mapper y) (* TODO: Is this really what I wanna do ? *)
let (@!) = apply_and_compose

let compose patch1 patch2 = {
  patch1 with (* TODO: write the rest *)
  expr = patch2.expr @! patch1.expr;
  pat = patch2.pat @! patch1.pat;
  structure_item = patch2.structure_item @! patch1.structure_item;
}

let (>>) = compose

let to_ppx m = fun _ -> m

let txt_is loc txt = loc.txt = txt

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
  let matches_pattern = function
    | { ppat_desc = Ppat_var loc } when txt_is loc fname -> true
    | _ -> false (* TODO: Is there another pattern to look at ? *)
  in
  let matches_binding = function
    | { pvb_pat = pat } when matches_pattern pat -> true
    | _ -> false
  in
  let transform_if_matches binding =
    if matches_binding binding then
      let pattern = {
        ppat_desc = Ppat_var { txt = arg_name; loc = binding.pvb_loc };
        ppat_loc = binding.pvb_loc;
        ppat_attributes = [];
      }
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
      fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_let (isrec, bindings, e) } ->
          { expr with
            pexp_desc = Pexp_let (isrec, List.map transform_if_matches bindings, e)
          }
        | e -> default_mapper.expr mapper e
  }

(* TODO: understand what I'm doing and remove the ugly "@guard" annotation *)
let make_fun var_name default_arg = {
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

let () =
  let patch =
    default_mapper
    >> add_arg_fun "f" "x"
    >> rename_var "f" "z"
    >> rename_var "z" "k"
    >> make_fun "k" (Exp.constant (Const_int 2))
  in register "patch" (to_ppx patch)
