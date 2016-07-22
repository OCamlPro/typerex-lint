type patch_line =
  | EQUAL of string
  | ADD of string
  | REMOVE of string
  | SUBPATCH of t
and t = patch_line list

let inside expr = "[%__sempatch_inside " :: expr @ ["]"]
let report expr = "[%__sempatch_report " :: expr @ ["]"]
(* let replace expr replacement = "(" :: expr @ ") [@__sempatch_replace " *)
(*                                              :: replacement @ ["]"] *)
let maybe_replace expr _replacement _has_change = expr
(* if has_change then replace expr replacement *)
(* else expr *)

let to_patch_body p =
  let rec convert_line = function
    | [] -> ([], [], false)
    | l :: tl ->
      let (before, after, is_change) = convert_line tl in
      match l with
      | EQUAL l -> (l :: before, l :: after, is_change)
      | ADD l -> (before, l :: after, true)
      | REMOVE l -> (l :: before, after, true)
      | SUBPATCH p -> (convert_patch p @ before, after, is_change)
  and convert_patch p =
    let (before, after, has_change) = convert_line p in
    inside (
      report (
        maybe_replace before after has_change
      )
    )
  in
  let patch = (* convert_patch p *)
    let (before, after, has_change) = convert_line p in
    report (
      maybe_replace before after has_change
    )
  in
  Parser.parse_expression Lexer.token (Lexing.from_string
                                         (String.concat "\n" @@ patch)
                                      )
