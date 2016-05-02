(* module P = Parsetree *)

type t =
  | Expression of Parsetree.expression
  | Ident of string

let to_string =
  let open Pprintast in
  let to_string printer value =
    printer Format.str_formatter value;
    Format.flush_str_formatter ()
  in
  function
  | Expression e -> to_string expression e
  | Ident i -> i
