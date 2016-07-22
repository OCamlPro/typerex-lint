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

let zip o1 o2 = merge_inf SUMisc.pair o1 o2

let value default = function
  | None -> default
  | Some x -> x

let (|?) opt default = value default opt

let fold f init = let open SUFun in
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
end
