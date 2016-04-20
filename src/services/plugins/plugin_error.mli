type error =
  | Plugin_already_registered of string
  | Plugin_not_found of string

exception  Plugin_error of error

val to_string : error -> string
