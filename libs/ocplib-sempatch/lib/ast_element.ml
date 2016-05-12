type t =
  | Expression of Parsetree.expression
  | String of string
  | Pattern of Parsetree.pattern

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
