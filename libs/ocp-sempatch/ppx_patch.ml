open Ast_mapper

(* open Ast_helper *)
open Asttypes
open Parsetree


type t = Ast_mapper.mapper

let compose m1 m2 = {
  m1 with (* TODO: write the rest *)
  expr = (fun mapper expr -> m1.expr mapper expr |> m2.expr mapper);
  pat = (fun mapper pat -> m1.pat mapper pat |> m2.pat mapper);
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
    | _ -> false (* TODO: complete pattern-matching *)
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

let () =
  let patch =
    default_mapper
    >> add_arg_fun "f" "x"
    >> rename_var "f" "z"
    >> rename_var "z" "k"
  in register "patch" (to_ppx patch)
