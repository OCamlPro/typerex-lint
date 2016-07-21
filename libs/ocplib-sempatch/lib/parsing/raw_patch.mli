type patch_line =
  | EQUAL of string
  | ADD of string
  | REMOVE of string
  | SUBPATCH of t
and t = patch_line list

val to_patch_body : t -> Parsetree.expression
