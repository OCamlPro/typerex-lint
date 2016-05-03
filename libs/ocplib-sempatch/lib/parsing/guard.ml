type t =
  | Variable of string
  | Apply of string * t list
