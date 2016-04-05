type cat = Code | Typo | Interface

type t = {
  name : string;
  details : string;
  cat : cat;
}

val cat_to_string : cat -> string
