[@@@ocaml.warning "+9"]
open Std_utils

open Parsetree


type ast =
  | Expr of expression
  | Pat of pattern

type 'a t = ast -> ast -> 'a -> ('a -> 'a -> 'a) -> 'a

type 'a mapper = {
  t2_expr: 'a mapper -> expression -> expression -> 'a;
}

(* let t2_bindings f default self = *)
(*   List.foldmap2_exn f (self.t2_value_binding self) default *)

(* let t2_structure_item f default self { pstr_loc = loc; pstr_desc = desc } = *)
(*   match desc with *)
(*   | Pstr_eval (x, a) -> f (self.t2_expr self x) (self.t2_attributes self a) *)
(*   | Pstr_value (_, bindings) -> t2_bindings f default self bindings *)
(*   | Pstr_primitive p -> self.t2_value_description self p *)
(*   | Pstr_type types -> List.foldmap f (self.t2_type_declaration self) default types *)
(*   | Pstr_typext t -> self.t2_type_extension self t *)
(*   | Pstr_exception _ -> default (* Why aren't exception handled in mapper ? *) *)
(*   | Pstr_module m -> self.t2_module_binding self m *)
(*   | Pstr_recmodule ml -> List.foldmap f (self.t2_module_binding self) default ml *)
(*   | Pstr_modtype mt -> self.t2_module_type_declaration self mt *)
(*   | Pstr_open open_desc -> self.t2_open_description self open_desc *)
(*   | Pstr_class cl -> List.foldmap f (self.t2_class_declaration self) default cl *)
(*   | Pstr_class_type ctl -> List.foldmap f (self.t2_class_type_declaration self) default ctl *)
(*   | Pstr_include decl -> self.t2_include_declaration self decl *)
(*   | Pstr_attribute attr -> self.t2_attribute self attr *)
(*   | Pstr_extension (e, attrs) -> f (self.t2_attributes self attrs) (self.t2_extension self e) *)

let t2_expression f default self e1 e2 =
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_ident _, Pexp_ident _ -> default
  | Pexp_constant _, Pexp_constant _ -> default
  (* | Pexp_let (_, b1, e1), Pexp_let (_, b2, e2) -> f (t2_bindings f default self b1 b2) (self.t2_expr self e1 e2) *)
  (* | Pexp_function c1, Pexp_function c2 -> self.t2_cases self c1 c2 *)
  (* | Pexp_fun (_, default_val1, pattern1, e1), Pexp_fun (_, default_val2, pattern2, e2) -> *)
  (*   let default_traversed = (Option.merge_inf (fun v1 v2 -> self.t2_expr self v1 v2) default_val1 default_val2) |> Option.value default *)
  (*   in f default_traversed @@ f (self.t2_expr self e1 e2) (self.t2_pat self pattern1 pattern2) *)
  | Pexp_apply (e1, args1), Pexp_apply (e2, args2) -> List.foldmap2_exn f (fun x y ->  (self.t2_expr self) (snd x) (snd y)) (self.t2_expr self e1 e2) args1 args2
  (* | Pexp_match (e1, c1), Pexp_match(e2, c2) *)
  (* | Pexp_try (e1, c1), Pexp_try (e2, c2) -> f (self.t2_cases self c1 c2) (self.t2_expr self e1 e2) *)
  | Pexp_tuple e1, Pexp_tuple e2 -> List.foldmap2_exn f (self.t2_expr self) default e1 e2
  | Pexp_variant (_, e1), Pexp_variant (_, e2) -> Option.merge_inf (self.t2_expr self) e1 e2 |> Option.value default
  | Pexp_record (f1, m1), Pexp_record (f2, m2) ->
    let trav_model =  Option.merge_inf (self.t2_expr self) m1 m2 |> Option.value default in
    List.foldmap2_exn f (fun x y -> (self.t2_expr self) (snd x) (snd y)) trav_model f1 f2
  | Pexp_field (e1, _), Pexp_field (e2, _) -> self.t2_expr self e1 e2
  | Pexp_let _ , Pexp_let _
  | Pexp_match _ , Pexp_match _
  | Pexp_try _, Pexp_try _
  | Pexp_function _,  Pexp_function _
  | Pexp_fun _ , Pexp_fun _
  | Pexp_setfield _, Pexp_setfield _
  | Pexp_array _, Pexp_array _
  | Pexp_ifthenelse _, Pexp_ifthenelse _
  | Pexp_sequence _, Pexp_sequence _
  | Pexp_while _, Pexp_while _
  | Pexp_for _, Pexp_for _
  | Pexp_constraint _, Pexp_constraint _
  | Pexp_coerce _, Pexp_coerce _
  | Pexp_send _, Pexp_send _
  | Pexp_new _, Pexp_new _
  | Pexp_setinstvar _, Pexp_setinstvar _
  | Pexp_override _, Pexp_override _
  | Pexp_letmodule _, Pexp_letmodule _
  | Pexp_assert _, Pexp_assert _
  | Pexp_lazy _, Pexp_lazy _
  | Pexp_poly _, Pexp_poly _
  | Pexp_object _, Pexp_object _
  | Pexp_newtype _, Pexp_newtype _
  | Pexp_pack _, Pexp_pack _
  | Pexp_open _, Pexp_open _
  | Pexp_extension _, Pexp_extension _
    -> Printast.expression 0 Format.std_formatter e1; failwith "Non implemented part of expr"
  | _ -> default

let traverse f default = {
  t2_expr = t2_expression f default;
}

let from_mapper default mapper ast1 ast2 =
  match ast1, ast2 with
  | Expr e1, Expr e2 -> mapper.t2_expr mapper e1 e2
  | Pat _, Pat _ -> failwith "Non implemented"
  | _ -> default
