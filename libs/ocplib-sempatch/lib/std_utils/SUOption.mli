type 'a t = 'a option

val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit

val merge_sup : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val merge_inf : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val zip : 'a t -> 'b t -> ('a * 'b) t

val value: 'a -> 'a t -> 'a

val fold: ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a

val some : 'a -> 'a t
val none : 'a t
val some_if : bool -> 'a -> 'a t

val is_some : 'a t -> bool
val is_none : 'a t -> bool

val bind : 'a t -> ('a -> 'b t) -> 'b t

val to_list : 'a t -> 'a list

module Infix :
sig
  val (|?) : 'a t -> 'a -> 'a
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end
