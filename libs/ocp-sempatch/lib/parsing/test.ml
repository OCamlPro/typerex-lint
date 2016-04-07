open Std_utils

let test_progs = [
  "f x y";
  "x";
]

let in_file = open_in "test/sempatch.md"

let patches = Patch_parser.sempatch (Patch_lexer.read) (Lexing.from_channel in_file)

let results = [
  [ 1 ; 1 ; 3 ; 0; 1 ; 0; 2];
  [ 0; 1 ; 1 ; 0; 0; 0; 0]
]

let test_asts = List.map (fun s -> Parser.parse_expression Lexer.token (Lexing.from_string s)) test_progs

let apply ast patch =
  let patch = Parsed_patches.preprocess patch in
  Ast_pattern_matcher.match_ast Parsed_patches.(patch.header.expr_variables) (Parsed_patches.preprocess_src_expr ast) (Ast_traverser2.Expr Parsed_patches.(patch.body.before))
  |> List.length

let () =
  List.iter2
    (
      fun ast result ->
        List.map (apply ast) patches
        |> List.map2 (=) result
        |> List.iteri (Printf.printf "test %d : %B\n")
    )
    test_asts
    results;
  close_in in_file
