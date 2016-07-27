type t = {
  patch_name : string;
  substitutions : Substitution.t;
  location : Location.t option;
  current_location : Location.t;
}

let get_patch_name t = (t.patch_name)
let get_location t = t.location
let get_current_location t = t.current_location
let get_substitutions t = t.substitutions

let set_patch_name x t = { t with patch_name = x }
let set_location x t = { t with location = x }
let set_current_location x t = { t with current_location = x }
let set_substitutions x t = { t with substitutions = x }

let mk name substitutions location current_location = {
  patch_name = name;
  substitutions;
  location;
  current_location;
}
