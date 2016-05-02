type t = {
  patch_name : string;
  substitutions : Substitution.t;
  location : Location.t;
}

val get_patch_name : t -> string
val get_location : t -> Location.t
val get_substitutions : t -> Substitution.t

val mk : Parsed_patches.t -> Substitution.t -> Location.t -> t
