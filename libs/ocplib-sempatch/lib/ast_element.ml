type record_field = Longident.t Asttypes.loc * Parsetree.expression
open Parsetree
open Asttypes
[%%create_ast_element]
type t = Element.t
let to_string =
  (* let open Pprintast in *)
  (* let to_string printer value = *)
  (*   printer Format.str_formatter value; *)
  (*   Format.flush_str_formatter () *)
  (* in *)
  function
  (* | Expression e -> to_string expression e *)
  (* | String i -> i *)
  (* | Pattern p -> to_string pattern p *)
  | _ -> "???"

let from_structure e = Element.Structure e
