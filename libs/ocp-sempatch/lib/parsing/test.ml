open Std_utils

let test_progs = [
  "f",   [ "f"; "f"            ; "bar"; "f"];
  "x",   [ "x"; "((+) x) 1"    ; "bar"; "x"];
  "f x", [ "y"; "f (((+) x) 1)"; "bar"; "f x"];
  "fun x -> x", [ "fun x  -> x"; "fun x  -> ((+) x) 1"; "bar"; "foo"];
]

let in_file = open_in "test/sempatch.md"

let patches = Patch_parser.sempatch (Patch_lexer.read) (Lexing.from_channel in_file)

let string_to_expr s = Parser.parse_expression Lexer.token (Lexing.from_string s)
let expr_to_string e =
  Pprintast.expression Format.str_formatter e;
  Format.flush_str_formatter ()

(* let test_asts = List.map string_to_expr test_progs *)

let apply ast patch =
  let patch = Parsed_patches.preprocess patch in
  Ast_pattern_matcher.apply patch ast

let test patches (ast, expected_results) =
  let parsed_ast = string_to_expr ast in
  List.map2 (fun expect patch ->
      let result = expr_to_string (apply parsed_ast patch) in
      Option.some_if (expect <> result) result)
    expected_results patches

let () =
  let failure = ref false in
  List.map (test patches) test_progs
  |> List.iteri
       (fun i -> List.iteri
          (fun j ast_opt -> match ast_opt with
            | Some ast ->
              Printf.printf "Error applying patch %d at test %d : got " j i;
              failure := true;
              print_endline ast
            | None -> ()
          )
       );
  close_in in_file;
  if !failure
  then
    exit 1
  else
    exit 0
