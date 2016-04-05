type t = {
  min_identifier_len : int;
  max_identifier_len : int;
}

let default : t = {
  min_identifier_len = 2;
  max_identifier_len = 25;
}
