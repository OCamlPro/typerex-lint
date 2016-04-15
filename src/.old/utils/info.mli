type cat = Code | Typo | Interface

type t = {
  name : string;
  details : string;
  cat : cat;
}

val string_of_cat : cat -> string
