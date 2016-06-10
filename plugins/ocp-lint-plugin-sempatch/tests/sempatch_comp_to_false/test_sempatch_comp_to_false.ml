
let test_comp_to_false condition texpr fexpr =
  if condition = false then texpr
  else fexpr

let test_comp_to_not_false condition texpr fexpr =
  if condition != false then texpr
  else fexpr

let test_comp_to_phys_false condition texpr fexpr =
  if condition == false then texpr
  else fexpr
