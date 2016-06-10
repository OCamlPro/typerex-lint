
let test_comp_to_true condition texpr fexpr =
  if condition = true then texpr
  else fexpr

let test_comp_to_not_true condition texpr fexpr =
  if condition != true then texpr
  else fexpr

let test_comp_to_phys_true condition texpr fexpr =
  if condition == true then texpr
  else fexpr
