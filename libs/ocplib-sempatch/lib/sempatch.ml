open Std_utils

module Ast_element = Ast_element
module Substitution = Substitution

module Match =
struct
  include Match
  let get_location m =
    Match.get_location m
    |> Option.value Location.none
end

module Patch =
struct
  open Parsed_patches.Type
  type t = patch

  let from_channel chan =
    Patch_parser.sempatch
      (Patch_lexer.read)
      (Lexing.from_channel chan)
  |> List.map snd
  |> List.map Parsed_patches.preprocess

  let get_name p = p.header.name
  let get_msg p = p.header.message
  let get_metavariables p = p.header.meta_expr

  let apply patch ast = let open Ast_element in
    match ast with
    | Expression e ->
      let
        results = Ast_pattern_matcher.apply patch e
      in
      List.map snd
        results
      |> List.filter (fun mat ->
          try
            Guard_evaluator.eval_union
              (Match.get_substitutions mat)
              patch.header.guard
          with Guard_evaluator.Undefined_var _ -> false
        )

    | _ -> assert false

  let parallel_apply patches tree =
    List.map (fun patch -> apply patch tree) patches
    |> List.concat

  let sequential_apply patches ast_elt =
    parallel_apply patches ast_elt
    (* List.fold_left (fun (accu_ast, accu_matches) patch -> *)
    (*     let (new_ast, new_matches) = apply patch accu_ast in *)
    (*     new_ast, new_matches @ accu_matches *)
    (*   ) *)
    (*   (ast_elt, []) *)
    (*   patches *)

end

module Failure = Failure
