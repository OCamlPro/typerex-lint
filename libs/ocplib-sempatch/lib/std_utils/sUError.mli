type ('good, 'bad) t =
  | Ok of 'good
  | Error of 'bad

val return : 'a -> ('a, 'err) t
val fail : 'a -> ('ok, 'a) t

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
val map_err : ('a -> 'b) -> ('ok, 'a) t -> ('ok, 'b) t

val bind : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t
val bind_err : ('a -> ('ok, 'b) t) -> ('ok, 'a) t -> ('ok, 'b) t

val ok_if : bool -> 'a -> 'b -> ('a, 'b) t

module Ok_monad_infix :
sig
  val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val (>|=) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
end
module Err_monad_infix :
sig
  val (>|=) : ('ok, 'a) t -> ('a -> 'b) -> ('ok, 'b) t
  val (>>=) : ('ok, 'a) t -> ('a -> ('ok, 'b) t) -> ('ok, 'b) t
end
