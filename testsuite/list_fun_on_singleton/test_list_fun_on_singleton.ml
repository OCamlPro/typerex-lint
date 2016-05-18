
let hd expr =
  List.hd [expr; expr];
  List.hd [expr];
  List.hd []

let tl expr =
  List.tl [expr; expr];
  List.tl [expr];
  List.tl []

let rev expr =
  List.rev [expr; expr];
  List.rev [expr];
  List.rev []

let concat () =
  List.concat []

let flatten () =
  List.flatten []

let map app expr =
  List.map app [expr; expr];
  List.map app [expr];
  List.map app []

let mapi app expr =
  List.mapi app [expr; expr];
  List.mapi app [expr];
  List.mapi app []

let iter app expr =
  List.iter app [expr; expr];
  List.iter app [expr];
  List.iter app []

let iteri app expr =
  List.iteri app [expr; expr];
  List.iteri app [expr];
  List.iteri app []

let for_all app expr =
  List.for_all app [expr; expr];
  List.for_all app [expr];
  List.for_all app []

let exists app expr =
  List.exists app [expr; expr];
  List.exists app [expr];
  List.exists app []

let mem app expr =
  List.mem app [expr; expr];
  List.mem app [expr];
  List.mem app []

let memq app expr =
  List.memq app [expr; expr];
  List.memq app [expr];
  List.memq app []

let find app expr =
  List.find app [expr; expr];
  List.find app [expr];
  List.find app []

let filter app expr =
  List.filter app [expr; expr];
  List.filter app [expr];
  List.filter app []

let find_all app expr =
  List.find_all app [expr; expr];
  List.find_all app [expr];
  List.find_all app []

let assoc app expr =
  List.assoc app [expr; expr];
  List.assoc app [expr];
  List.assoc app []

let assq app expr =
  List.assq app [expr; expr];
  List.assq app [expr];
  List.assq app []

let mem_assoc app expr =
  List.mem_assoc app [expr; expr];
  List.mem_assoc app [expr];
  List.mem_assoc app []

let mem_assq app expr =
  List.mem_assq app [expr; expr];
  List.mem_assq app [expr];
  List.mem_assq app []

let remove_assoc app expr =
  List.remove_assoc app [expr; expr];
  List.remove_assoc app [expr];
  List.remove_assoc app []

let remove_assq app expr =
  List.remove_assq app [expr; expr];
  List.remove_assq app [expr];
  List.remove_assq app []

let sort app expr =
  List.sort app [expr; expr];
  List.sort app [expr];
  List.sort app []

let stable_sort app expr =
  List.stable_sort app [expr; expr];
  List.stable_sort app [expr];
  List.stable_sort app []

let fast_sort app expr =
  List.fast_sort app [expr; expr];
  List.fast_sort app [expr];
  List.fast_sort app []

let sort_uniq app expr =
  List.sort_uniq app [expr; expr];
  List.sort_uniq app [expr];
  List.sort_uniq app []

let fold_left acc app expr =
  List.fold_left app acc [expr; expr];
  List.fold_left app acc [expr];
  List.fold_left app acc []

let fold_right acc app expr =
  List.fold_right app [expr; expr] acc;
  List.fold_right app [expr] acc;
  List.fold_right app [] acc

let split expr =
  List.split [expr; expr];
  List.split [expr];
  List.split []

let string_concat sep elt =
  String.concat sep [elt; elt];
  String.concat sep [elt];
  String.concat sep []
