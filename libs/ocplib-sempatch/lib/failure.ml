type t =
  | Lexing of Lexing.position
  | Patch of string
  | Guard of string
  | Replacement of Location.t
  | Non_implemented of Location.t

exception SempatchException of t

let in_file_pos_to_string pos =
  Lexing.(Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol))
let position_to_string pos =
  Lexing.(Printf.sprintf
            "file %s at position %s"
            (if pos.pos_fname = "" then "NONE" else pos.pos_fname)
            (in_file_pos_to_string pos)
         )

let to_string = function
  | Lexing pos -> "Lexing error at " ^ (position_to_string pos)
  | Patch err -> "Parsing error : " ^ err
  | Non_implemented pos -> "Non implemented from "
                           ^ (position_to_string pos.Location.loc_start)
                           ^ "-"
                           ^ (in_file_pos_to_string pos.Location.loc_end)
  | Guard err -> "Guard error : " ^ err
  | Replacement pos ->
    "Illegal use of the ast extension [@__sempatch_replace] at "
    ^ (position_to_string pos.Location.loc_start)
