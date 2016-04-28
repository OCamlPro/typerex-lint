(**
   The type of semantic patches
*)
type t = Parsed_patches.t

(**
   Parse the given channel and return the corresponding list of patches
*)
val from_channel : in_channel -> t MyStringMap.t

(**
   Parse the given channel as a patch_body
*)
val parse_body : in_channel -> Parsed_patches.body

(**
   Creates a patch out of the given name, body and headers
*)
val mk : Parsed_patches.body -> Parsed_patches.header -> t

(**
   Apply the patch to the expression and return the resulting expression
*)
val apply : t -> Parsetree.expression -> Parsetree.expression

(**
   Return all the matches on the given expression
*)
val get_matches_from_patch : t -> Parsetree.expression -> (Variables.env * Location.t) list

(**
   Return all the matches from all the patches on the given expression
*)
val get_matches_from_patches : t MyStringMap.t -> Parsetree.expression -> (string * (Variables.env * Location.t)) list
