type t = {
  patch_name : string;
  substitutions : Substitution.t;
  location : Location.t option;
  current_location : Location.t;
}

val get_patch_name : t -> string
val get_location : t -> Location.t option
val get_current_location : t -> Location.t
val get_substitutions : t -> Substitution.t

val set_patch_name : string -> t -> t
val set_location : Location.t option -> t -> t
val set_current_location : Location.t -> t -> t
val set_substitutions : Substitution.t -> t -> t

val mk : string -> Substitution.t -> Location.t option -> Location.t -> t
