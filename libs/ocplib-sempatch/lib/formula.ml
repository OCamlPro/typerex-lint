type 'a t =
  | Var of 'a
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Not of 'a t
