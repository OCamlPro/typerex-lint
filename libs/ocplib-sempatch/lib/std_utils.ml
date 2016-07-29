type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

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

  val compose_binop : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c

  val id : 'a -> 'a
end
=
struct
  let flip f x y = f y x
  let compose f g x = f (g x)

  let ( %> ) = compose

  let compose_binop op f x1 x2 = op (f x1) (f x2)

  let id x = x
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

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a

  val some : 'a -> 'a t
  val none : 'a t
  val some_if : bool -> 'a -> 'a t

  val is_some : 'a t -> bool
  val is_none : 'a t -> bool

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val to_list : 'a t -> 'a list

  module Infix :
  sig
    val (|?) : 'a t -> 'a -> 'a
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val (>||) : 'a t -> 'b t -> ('a * 'b) t
  end
end
=
struct
  type 'a t = 'a option

  let bind x f = match x with
    | None -> None
    | Some x -> f x

  let map f x = bind x (fun x -> Some (f x))

  let iter f x = ignore (map f x)

  let merge_sup f o1 o2 = match o1, o2 with
    | Some x, Some y -> Some (f x y)
    | Some x, _ -> Some x
    | _ -> o2

  let merge_inf f o1 o2 = match o1, o2 with
    | Some x, Some y -> Some (f x y)
    | _ -> None

  let zip o1 o2 = merge_inf Misc.pair o1 o2

  let both = zip

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

  let to_list = function
    | Some x -> [x]
    | None -> []

  module Infix =
  struct
    let (|?) = (|?)
    let (>>=) = bind
    let (>|=) x y = map y x
    let (>||) = both
  end
end

module Error:
sig
  type ('good, 'bad) t = ('good, 'bad) result

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
end
=
struct
  type (+'good, +'bad) t = ('good, 'bad) result

  let return x = Ok x
  let fail x = Error x

  let map2 f_ok f_fail = function
    | Ok x -> Ok (f_ok x)
    | Error e -> Error (f_fail e)

  let map f = map2 f Fun.id
  let map_err f x = map2 Fun.id f x

  let bind2 f_ok f_fail = function
    | Ok x -> f_ok x
    | Error e -> f_fail e

  let bind f = bind2 f fail
  let bind_err f x = bind2 return f x

  let ok_if cond ok_val err_val =
    if cond then Ok ok_val else Error err_val

  module Ok_monad_infix =
  struct
    let (>>=) x f = bind f x
    let (>|=) x f = map f x
  end
  module Err_monad_infix =
  struct
    let (>>=) x f = bind_err f x
    let (>|=) x f = map_err f x
  end
end

module UList:
sig
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
end
=
struct
  include List

  type 'a t = 'a list

  let foldmap foldFun mapFun ini =
    Fun.compose
      (List.fold_left foldFun ini)
      (List.map mapFun)

  let foldmap2_exn foldFun mapFun ini l =
    Fun.compose
      (List.fold_left foldFun ini)
      (List.map2 mapFun l)

  let cons e l = e::l

  let sum = map2

  let rec take n l = match n, l with
    | 0, _ -> Some []
    | n, (hd::tl) when n > 0 ->
      Option.bind (take (n-1) tl) (fun l -> Some (cons hd l))
    | _ -> None

  let truncate_as l1 l2 =
    let new_length = List.length l2 in
    take new_length l1

  let bind f lst = List.map f lst |> List.flatten
  let product_bind f l1 l2 = bind (fun x -> bind (f x) l2) l1
  let product f l1 l2 = product_bind (fun x y -> [f x y]) l1 l2

  let flip_opt list = List.fold_left (fun accu elt ->
      match accu, elt with
      | Some acc, Some el -> Some (el::acc)
      | _ -> None
    )
      (Some [])
      list

  let find_opt f l = try List.find f l |> Option.some with Not_found -> None
end

module StringMap:
sig
  include Map.S with type key = string
  val from_list_pair : (string * 'a) list -> 'a t
  val get : string -> 'a t -> 'a option
end
=
struct
  module M = Map.Make(String)
  include M

  let from_list_pair l =
    List.fold_left
      (fun map (k, e) -> M.add k e map)
      M.empty
      l

  let get k sm =
    try
      Some (M.find k sm)
    with Not_found -> None
end

module Messages :
sig
  val debug : ('a, unit, string, unit) format4 -> 'a
  val warn : ('a, unit, string, unit) format4 -> 'a
end
=
struct
  let out_fun =
    try
      ignore @@ Sys.getenv "OCAML_VERBOSE";
      output_string stderr
    with Not_found -> ignore
  let debug msg = Printf.ksprintf
      (fun msg -> out_fun ("Debug : " ^ msg))
      msg
  let warn msg = Printf.ksprintf
      (fun msg -> out_fun ("Warning : " ^ msg))
      msg
end

module List = UList
