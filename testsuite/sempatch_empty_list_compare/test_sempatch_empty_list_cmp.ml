
let test_empty_list_cmp list texpr fexpr =
  if List.length list = 0 then texpr
  else fexpr
