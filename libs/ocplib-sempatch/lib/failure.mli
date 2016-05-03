type t =
  | Lexing of Lexing.position
  | Patch of string
  | Guard of string
  | Non_implemented of Location.t

exception SempatchException of t

val to_string : t -> string
