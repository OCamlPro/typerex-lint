open Std_utils

type t = {
  patch_name : string;
  substitutions : Substitution.t;
  location : Location.t;
}

let get_patch_name t = t.patch_name
let get_location t = t.location
let get_substitutions t = t.substitutions

let mk patch substitutions location = {
  patch_name = Option.value "" Parsed_patches.Type.(patch.header.name);
  substitutions;
  location;
}
