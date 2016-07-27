open Std_utils

module Ast_element = Ast_element
module Substitution = Substitution

module P = Parsed_patches

module Patch =
struct
  include Parsed_patches

  let from_channel chan =
    Patch_parser.sempatch
      Patch_lexer.read
      (Lexing.from_channel chan)
    |> List.map snd
    |> List.map Parsed_patches.preprocess

  let get_name p = p.header.name
  let get_metavariables p = p.header.meta_expr
  let get_field field p = StringMap.get field p.header.keyvals
  let get_msg = get_field "message"

  let apply patch ast = let open Ast_element in
    match ast with
    | Element.Structure e ->
      let
        results = Eval.apply (P.get_name patch) (P.get_body patch)
          (Element.Structure e)
      in
      List.map snd
        results
      |> List.filter (fun mat ->
          (Option.is_some (Match.get_location mat)) &&
          try
            Guard_evaluator.eval_union
              (Match.get_substitutions mat)
              (P.get_guard patch)
          with Guard_evaluator.Undefined_var _ -> false
        )
    | Element.Expression e ->
      let
        results = Eval.apply (P.get_name patch) (P.get_body patch)
          (Element.Expression e)
      in
      results
      |> List.map snd
      |> List.filter (fun mat ->
          (Option.is_some (Match.get_location mat)) &&
          try
            let res = Guard_evaluator.eval_union
                (Match.get_substitutions mat)
                (P.get_guard patch)
            in
            if not res then
              begin
                Messages.debug "failed guard\n";
                Messages.debug "> The variables where :\n";
                StringMap.iter (fun name value ->
                    match value with
                    | Ast_element.Element.Expression e ->
                      Messages.debug "> %s : %s\n"
                        name
                        (Pprintast.expression Format.str_formatter e;
                         Format.flush_str_formatter ())
                    | _ -> ()
                  )
                  (Match.get_substitutions mat)
              end;
            res
          with Guard_evaluator.Undefined_var var ->
            Messages.debug "undefined var %s\n" var;
            false
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
