module Misc:
sig
  val pair: 'a -> 'b -> 'a*'b

  val const: 'a -> 'b -> 'a
end
=
struct
  let pair a b = a,b

  let const x _ = x
end

module Fun:
sig
  val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

  val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val ( %> ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
end
=
struct
  let flip f x y = f y x
  let compose f g x = f (g x)

  let ( %> ) = compose
end

module Option:
sig
  type 'a t = 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit

  val merge_sup : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge_inf : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val value: 'a -> 'a t -> 'a

  val (|?) : 'a t -> 'a -> 'a

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a

  val some : 'a -> 'a t
  val none : 'a t
  val some_if : bool -> 'a -> 'a t

  val is_some : 'a t -> bool
  val is_none : 'a t -> bool

  module Infix :
  sig
    val (|?) : 'a t -> 'a -> 'a
  end
end
=
struct
  type 'a t = 'a option

  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let iter f x = ignore (map f x)

  let merge_sup f o1 o2 = match o1, o2 with
    | Some x, Some y -> Some (f x y)
    | Some x, _ -> Some x
    | _ -> o2

  let merge_inf f o1 o2 = match o1, o2 with
    | Some x, Some y -> Some (f x y)
    | _ -> None

  let zip o1 o2 = merge_inf Misc.pair o1 o2

  let value default = function
    | None -> default
    | Some x -> x

  let (|?) opt default = value default opt

  let fold f init = let open Fun in
    value init %> map (f init)

  let some x = Some x
  let none = None

  let some_if cond x = if cond then Some x else None

  let is_none x = (=) None x
  let is_some x = (<>) None x

  module Infix =
  struct
    let (|?) = (|?)
  end
end

module UList:
sig
  include module type of List

  type 'a t = 'a list

  val foldmap: ('c -> 'b -> 'c) -> ('a -> 'b) -> 'c -> 'a t -> 'c
  val foldmap2_exn: ('c -> 'b -> 'c) -> ('a -> 'd -> 'b) -> 'c -> 'a t -> 'd t -> 'c

  val cons : 'a -> 'a list -> 'a list

  val sum : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val product : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

  val find_opt : ('a -> bool) -> 'a list -> 'a option
end
=
struct
  include List

  type 'a t = 'a list

  let foldmap foldFun mapFun ini = Fun.compose (List.fold_left foldFun ini) (List.map mapFun)

  let foldmap2_exn foldFun mapFun ini l = Fun.compose (List.fold_left foldFun ini) (List.map2 mapFun l)

  let cons e l = e::l

  let sum = map2

  let product f l1 l2 = List.map (fun x -> List.map (f x) l2) l1 |> List.flatten

  let find_opt f l = try List.find f l |> Option.some with Not_found -> None
end

module List = UList
