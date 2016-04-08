let f a b = a + b

let very_very_very_long_long_variable_name1 = 2

let very_very_very_long_long_variable_name2 = 3

let _ =
  let very_long_variable_name1 = 2 in
  let very_long_variable_name2 = 3 in
  let pattern = Some 1 in
  match pattern with
  | Some very_long_variable_name3 -> ()
  | None -> ()
  
