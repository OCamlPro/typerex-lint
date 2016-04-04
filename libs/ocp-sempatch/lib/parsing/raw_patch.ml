type id = string
   
type header = {
  expr_variables : string list;
}

type patch_line =
  | EQUAL of string
  | ADD of string
  | REMOVE of string

type body = patch_line list

type t = id * header * body

