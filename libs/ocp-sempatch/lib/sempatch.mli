(**
   The type of semantic patches
*)
type t

(**
   Parse the given channel and return the corresponding list of patches
*)
val from_channel : in_channel -> t list

(**
   Parse the given channel as a patch_body
*)
val parse_body : in_channel -> Parsed_patches.body

(**
   Creates a patch out of the given name, body and headers
*)
val mk : string -> Parsed_patches.body -> Parsed_patches.header -> t

(**
   Apply the patch to the expression and return the resulting expression
*)
val apply : t -> Parsetree.expression -> Parsetree.expression

val get_name : t -> string
