open Std_utils

module Ast_element = Ast_element
module Substitution = Substitution

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
    | Structure e ->
      let
        results = Eval.apply patch.header.name patch.body
          (Ast_element.Structure (Parsed_patches.preprocess_src_expr e))
      in
      List.map snd
        results
      |> List.filter (fun mat ->
          (Option.is_some (Match.get_location mat)) &&
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

end

module Match =
struct
  include Match
  let get_location m =
    Match.get_location m
    |> Option.value Location.none
end

module Failure = Failure
