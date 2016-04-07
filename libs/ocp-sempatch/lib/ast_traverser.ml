[@@@ocaml.warning "+9"]
open Std_utils

open Parsetree

type 'a t = {
  traverse_expr : 'a t -> Parsetree.expression -> 'a;
}

(* let traverse_bindings f default self = *)
(*   List.foldmap f (self.traverse_value_binding self) default *)

(* let traverse_structure_item f default self { pstr_loc = loc; pstr_desc = desc } = *)
(*   match desc with *)
(*   | Pstr_eval (x, a) -> f (self.traverse_expr self x) (self.traverse_attributes self a) *)
(*   | Pstr_value (_, bindings) -> traverse_bindings f default self bindings *)
(*   | Pstr_primitive p -> self.traverse_value_description self p *)
(*   | Pstr_type types -> List.foldmap f (self.traverse_type_declaration self) default types *)
(*   | Pstr_typext t -> self.traverse_type_extension self t *)
(*   | Pstr_exception _ -> default (* Why aren't exception handled in mapper ? *) *)
(*   | Pstr_module m -> self.traverse_module_binding self m *)
(*   | Pstr_recmodule ml -> List.foldmap f (self.traverse_module_binding self) default ml *)
(*   | Pstr_modtype mt -> self.traverse_module_type_declaration self mt *)
(*   | Pstr_open open_desc -> self.traverse_open_description self open_desc *)
(*   | Pstr_class cl -> List.foldmap f (self.traverse_class_declaration self) default cl *)
(*   | Pstr_class_type ctl -> List.foldmap f (self.traverse_class_type_declaration self) default ctl *)
(*   | Pstr_include decl -> self.traverse_include_declaration self decl *)
(*   | Pstr_attribute attr -> self.traverse_attribute self attr *)
(*   | Pstr_extension (e, attrs) -> f (self.traverse_attributes self attrs) (self.traverse_extension self e) *)

let traverse_expression f default self e =
  match e.pexp_desc with
  | Pexp_ident _ -> default
  | Pexp_constant _ -> default
  (* | Pexp_let (_, bindings, expr) -> f (traverse_bindings f default self bindings) (self.traverse_expr self expr) *)
  (* | Pexp_function cases -> self.traverse_cases self cases *)
  | Pexp_fun (_, default_val, _, _) -> (Option.fold (fun _ v -> self.traverse_expr self v) default default_val)
  | Pexp_apply (e, args) -> List.foldmap f (Fun.compose (self.traverse_expr self) snd) (self.traverse_expr self e) args
  (* | Pexp_match (e, cases) *)
  (* | Pexp_try (e, cases) -> f (self.traverse_cases self cases) (self.traverse_expr self e) *)
  | Pexp_tuple exprs -> List.foldmap f (self.traverse_expr self) default exprs
  | Pexp_construct (_, expr)
  | Pexp_variant (_, expr) -> Option.fold (fun _ -> self.traverse_expr self) default expr
  | Pexp_record (fields, model) ->
    let trav_model =  Option.fold (fun _ -> self.traverse_expr self) default model in
    List.foldmap f (Fun.compose (self.traverse_expr self) snd) trav_model fields
  | Pexp_field (e, _) -> self.traverse_expr self e
  | _ -> failwith "Non implemented"

let traverse f default = {
  (* traverse_structure = (fun self l -> List.foldmap f (self.traverse_structure_item self) default l); *)
  (* traverse_structure_item = traverse_structure_item f default; *)
  traverse_expr = traverse_expression f default;
}

let apply_to_expr f default trav2 ast self e =
  f (trav2 (Ast_traverser2.Expr e) ast) (traverse_expression f default self e)

let apply_traverser2 f default trav2 ast = {
  traverse_expr = apply_to_expr f default trav2 ast;
}
