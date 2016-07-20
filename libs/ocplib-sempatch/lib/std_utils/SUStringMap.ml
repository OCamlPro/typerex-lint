module M = Map.Make(String)
include M

let from_list_pair l =
  List.fold_left
    (fun map (k, e) -> M.add k e map)
    M.empty
    l

