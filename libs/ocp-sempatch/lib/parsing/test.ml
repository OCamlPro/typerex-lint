open Std_utils

let test_progs = [
  "x", [ "simpleVar", "y" ];
  "f x", [ "apply", "foo"];
  "x", [ "patch1", "((+) x) 1" ];
  "fun x -> x", [ "patch1", "fun x  -> ((+) x) 1"; "functionMatch", "foo"];
  "let x = 1 in x", [ "letBinding", "tralala"; "replaceInsideLet", "let x = 1 in y"];
]

let in_file = open_in "test/sempatch.md"

let patches = Patch_parser.sempatch (Patch_lexer.read_all) (Lexing.from_channel in_file)

let string_to_expr s = Parser.parse_expression Lexer.token (Lexing.from_string s)
let expr_to_string e =
  Pprintast.expression Format.str_formatter e;
  Format.flush_str_formatter ()

let apply ast patch =
  let patch = Parsed_patches.preprocess patch in
  Ast_pattern_matcher.apply patch ast

let test patches (ast, expected_results) =
  let parsed_ast = string_to_expr ast in
  List.map (fun patch ->
    List.map (fun (name, expected) ->
        if (name = patch.Parsed_patches.name) then
          let result = expr_to_string (apply parsed_ast patch) in
          Option.some_if (expected <> result) (name, result)
        else None
        )
    expected_results
  )
  patches
  |> List.flatten

let () =
  let failure = ref false in
  List.map (test patches) test_progs
  |> List.iteri
       (fun i -> List.iter
          (fun ast_opt -> match ast_opt with
            | Some (name, ast) ->
              Printf.printf "Error applying patch %s at test %d : got " name i;
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
