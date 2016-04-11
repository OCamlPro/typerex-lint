open Std_utils

let test_progs = [
  "f";
  "x";
]

let in_file = open_in "test/sempatch.md"

let patches = Patch_parser.sempatch (Patch_lexer.read) (Lexing.from_channel in_file)

let results = [
  [ 1 ; 1 ; 5 ; 0; 1 ; 0; 2];
  [ 0; 1 ; 1 ; 0; 0; 0; 0]
]

let test_asts = List.map (fun s -> Parser.parse_expression Lexer.token (Lexing.from_string s)) test_progs

let apply ast patch =
  let patch = Parsed_patches.preprocess patch in
  Ast_pattern_matcher.apply patch ast
  (* |> List.length *)

let () =
  List.iter2
    (
      fun ast _result ->
        List.map (apply ast) patches
        |> List.iter @@ Printast.expression 0 Format.std_formatter
        (* |> List.map2 (=) result *)
        (* |> List.iteri (Printf.printf "test %d : %B\n") *)
    )
    test_asts
    results;
  close_in in_file
