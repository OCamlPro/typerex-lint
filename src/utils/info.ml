type cat = Code | Typo | Interface

type t = {
  name : string;
  details : string;
  cat : cat;
}

let string_of_cat = function
  | Code -> "Code"
  | Typo -> "Typo"
  | Interface -> "Interface"
