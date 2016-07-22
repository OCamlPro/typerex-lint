include module type of List

type 'a t = 'a list

val foldmap: ('c -> 'b -> 'c) -> ('a -> 'b) -> 'c -> 'a t -> 'c
val foldmap2_exn:
  ('c -> 'b -> 'c) ->
  ('a -> 'd -> 'b) ->
  'c -> 'a t -> 'd t -> 'c

val cons : 'a -> 'a list -> 'a list

val truncate_as : 'a list -> 'b list -> 'a list option

val bind : ('a -> 'b t) -> 'a t -> 'b t
val sum : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val product : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val product_bind : ('a -> 'b -> 'c t) -> 'a list -> 'b list -> 'c list

val flip_opt : 'a option list -> 'a list option

val find_opt : ('a -> bool) -> 'a list -> 'a option
