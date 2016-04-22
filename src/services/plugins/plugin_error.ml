
type error =
  | Plugin_already_registered of (module Plugin_types.PLUGIN)
  | Plugin_not_found of (module Plugin_types.PLUGIN)

exception Plugin_error of error

let spf = Printf.sprintf

let to_string = function
  | Plugin_already_registered plugin ->
    let module P = (val plugin : Plugin_types.PLUGIN) in
    spf "Plugin '%s' is already registered." P.name
  | Plugin_not_found plugin ->
    let module P = (val plugin : Plugin_types.PLUGIN) in
    spf "Plugin '%s' is not found." P.name
