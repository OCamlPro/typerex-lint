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
  Ast_pattern_matcher.apply patch expression
  |> Res.map fst
  |> Res.unwrap

let get_matches patch expression =
  Ast_pattern_matcher.apply patch expression
  |> Res.map snd
  |> Res.unwrap
  |> (fun x -> x.Variables.matches)
