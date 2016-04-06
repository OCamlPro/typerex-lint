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

  val merge_sup : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge_inf : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val value: 'a -> 'a t -> 'a

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
end
=
struct
  type 'a t = 'a option

  let map f = function
    | None -> None
    | Some x -> Some (f x)

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

  let fold f init = let open Fun in
    value init %> map (f init)
end

module UList:
sig
  include module type of List

  type 'a t = 'a list

  val foldmap: ('c -> 'b -> 'c) -> ('a -> 'b) -> 'c -> 'a t -> 'c
  val foldmap2_exn: ('c -> 'b -> 'c) -> ('a -> 'd -> 'b) -> 'c -> 'a t -> 'd t -> 'c
end
=
struct
  include List

  type 'a t = 'a list

  let foldmap foldFun mapFun ini = Fun.compose (List.fold_left foldFun ini) (List.map mapFun)

  let foldmap2_exn foldFun mapFun ini l = Fun.compose (List.fold_left foldFun ini) (List.map2 mapFun l)
end

module List = UList
