type patch_line =
  | EQUAL of string
  | ADD of string
  | REMOVE of string

type t = patch_line list

let rec filter_map f = function
  | [] -> []
  | hd::tl ->
    match f hd with
    | Some hd' -> hd'::(filter_map f tl)
    | None -> (filter_map f tl)

let exists_before_patch = function
  | EQUAL l | REMOVE l -> Some l
  | ADD _ -> None

let exists_after_patch = function
    | EQUAL l | ADD l -> Some l
    | REMOVE _ -> None

let to_patch_body patch =
  let parse_strlist expr =
  Parser.parse_expression Lexer.token (Lexing.from_string (String.concat "\n" expr))
  in
  {
    Parsed_patches.before = parse_strlist (filter_map exists_before_patch patch);
    Parsed_patches.after = parse_strlist (filter_map exists_after_patch patch);
  }
