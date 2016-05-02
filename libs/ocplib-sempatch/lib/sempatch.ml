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

  let apply patch ast = let open Ast_element in
    match ast with
    | Expression e -> let e, m = Ast_pattern_matcher.apply patch e |>Res.unwrap in Expression e, m
    | Ident _ -> assert false

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
end
(* include Parsed_patches.Type *)
(*  *)
(* module StringMap = StringMap *)
(*  *)
(* type t = Parsed_patches.t *)
(*  *)
(* let from_channel chan = *)
(*   Patch_parser.sempatch *)
(*     (Patch_lexer.read) *)
(*     (Lexing.from_channel chan) *)
(*   |> StringMap.from_list_pair *)
(*  *)
(* let parse_body chan = *)
(*   Code_parser.code *)
(*     (Code_lexer.read_code) *)
(*     (Lexing.from_channel chan) *)
(*   |> Raw_patch.to_patch_body *)
(*  *)
(* let mk body header = ({ body; header; }) *)
(*  *)
(* let apply patch expression = *)
(*   Ast_pattern_matcher.apply patch expression *)
(*   |> Res.map fst *)
(*   |> Res.unwrap *)
(*  *)
(* let get_matches_from_patch patch expression = *)
(*   Ast_pattern_matcher.apply patch expression *)
(*   |> Res.map snd *)
(*   |> Res.unwrap *)
(*   |> (fun x -> x.Environment.matches) *)
(*  *)
(* let get_matches_from_patches patches expression= *)
(*   StringMap.fold (fun name patch accu -> *)
(*       List.map (fun res -> name, res) (get_matches_from_patch patch expression) @ accu *)
(*     ) *)
(*     patches *)
(*     [] *)
