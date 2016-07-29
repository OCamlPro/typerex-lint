type t =
  | Variable of string
  | Litt_integer of int
  | Apply of string * t list
