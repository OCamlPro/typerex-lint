type record_field = Longident.t Asttypes.loc * Parsetree.expression
open Parsetree
open Asttypes
[%%build_tree_type]
type t = tree
(* type t = *)
(*   | Expression of Parsetree.expression *)
(*   | Expressions of Parsetree.expression list *)
(*   | Expression_opt of Parsetree.expression option *)
(*   | String of string *)
(*   | Pattern of Parsetree.pattern *)
(*   | Pattern_opt of Parsetree.pattern option *)
(*   | Value_binding of Parsetree.value_binding *)
(*   | Value_bindings of Parsetree.value_binding list *)
(*   | Structure_item of Parsetree.structure_item *)
(*   | Structure of Parsetree.structure *)
(*   | Case of Parsetree.case *)
(*   | Cases of Parsetree.case list *)
(*   | Record_field of record_field *)
(*   | Record_fields of record_field list *)

let to_string =
  let open Pprintast in
  let to_string printer value =
    printer Format.str_formatter value;
    Format.flush_str_formatter ()
  in
  function
  | Expression e -> to_string expression e
  | String i -> i
  | Pattern p -> to_string pattern p
  | _ -> "???"

let from_structure e = Structure e
