open Std_utils

module Ast_element = Ast_element
module Substitution = Substitution
module Match = Match

module Patch =
struct
  open Parsed_patches.Type
  type t = patch

  let from_channel chan =
    Patch_parser.sempatch
      (Patch_lexer.read)
      (Lexing.from_channel chan)
  |> List.map snd

  let get_name p = p.header.name
  let get_msg p = p.header.message
  let get_metavariables p = p.header.meta_expr

  let apply' apply_fun patch ast = let open Ast_element in
    match ast with
    | Expression e ->
      let
        e, m = apply_fun patch e |> Res.unwrap
      in
      Expression e, m
    | Ident _ -> assert false

  let apply patch ast =
    apply' (Ast_pattern_matcher.apply true) patch ast

  let apply_nonrec patch ast =
    apply' (Ast_pattern_matcher.apply false) patch ast

  let sequential_apply patches ast_elt =
    List.fold_left (fun (accu_ast, accu_matches) patch ->
        let (new_ast, new_matches) = apply patch accu_ast in
        new_ast, new_matches @ accu_matches
      )
      (ast_elt, [])
      patches

  let parallel_apply patches tree =
    List.map (fun patch -> apply patch tree |> snd) patches
    |> List.concat

  let parallel_apply_nonrec patches tree =
    List.map (fun patch -> apply_nonrec patch tree |> snd) patches
    |> List.concat
end

module Failure = Failure
