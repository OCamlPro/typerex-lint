(** Type [error] is used to handle all internal errors for plugins. *)
type error =
  | Plugin_already_registered of (module Plugin_types.PLUGIN)
  | Plugin_not_found of (module Plugin_types.PLUGIN)

(** [to_string err] returns a string representation of [err].  *)
val to_string : error -> string

exception Plugin_error of error
