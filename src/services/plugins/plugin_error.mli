type error =
  | Plugin_already_registered of (module Plugin_types.PLUGIN)
  | Plugin_not_found of (module Plugin_types.PLUGIN)

exception  Plugin_error of error

val to_string : error -> string
