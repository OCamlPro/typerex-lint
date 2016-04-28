type patch_line =
  | EQUAL of string
  | ADD of string
  | REMOVE of string
  | SUBPATCH of t
and t = patch_line list

let rec filter_map f = function
  | [] -> []
  | hd::tl ->
    match f hd with
    | Some hd' -> hd'::(filter_map f tl)
    | None -> (filter_map f tl)

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
    "[%__sempatch_inside (" :: before @ ")" :: (if has_change then " [@__sempatch_replace" :: after @ ["]"] else []) @ ["]"]
  in Parser.parse_expression Lexer.token (Lexing.from_string (String.concat "\n" @@ convert_patch p))
     |> (fun x -> x)
