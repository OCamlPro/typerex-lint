
let my_fun expr list =
  let _list = [] @ _list in
  let _list = [expr] @ _list in
  let _list = _list @ [] in
  let _list = _list @ [expr] in
  _list @ _list
