type t = Parsed_patches.t

let from_channel chan =
  Patch_parser.sempatch
    (Patch_lexer.read_all)
    (Lexing.from_channel chan)
  |> StringMap.from_list_pair

let parse_body chan =
  Code_parser.code
    (Code_lexer.read_code)
    (Lexing.from_channel chan)
  |> Raw_patch.to_patch_body

let mk body header = Parsed_patches.({ body; header; })

let apply patch expression =
  let patch = Parsed_patches.preprocess patch
  and expression = Parsed_patches.preprocess_src_expr expression
  in
  Ast_pattern_matcher.apply patch expression
  |> Parsed_patches.postprocess
