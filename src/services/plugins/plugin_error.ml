
type error =
  | Plugin_already_registered of string
  | Plugin_not_found of string

exception Plugin_error of error

let spf = Printf.sprintf

let to_string = function
  | Plugin_already_registered pname ->
    spf "Plugin '%s' is already registered." pname
  | Plugin_not_found pname ->
    spf "Plugin '%s' is not found." pname
