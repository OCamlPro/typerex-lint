include List

type 'a t = 'a list

let foldmap foldFun mapFun ini =
  SUFun.compose
    (List.fold_left foldFun ini)
    (List.map mapFun)

let foldmap2_exn foldFun mapFun ini l =
  SUFun.compose
    (List.fold_left foldFun ini)
    (List.map2 mapFun l)

let cons e l = e::l

let sum = map2

let rec take n l = match n, l with
  | 0, _ -> Some []
  | n, (hd::tl) when n > 0 ->
    SUOption.bind (take (n-1) tl) (fun l -> Some (cons hd l))
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

let find_opt f l = try List.find f l |> SUOption.some with Not_found -> None
