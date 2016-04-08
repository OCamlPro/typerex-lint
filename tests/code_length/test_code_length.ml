let a = 
  1 
  +                                                                               3

(* A very long comment                                                           *)

let long_exp_in_fun = function
  | Some _ -> "this is a very long string which should trigger a warning in code_length"
  | None -> ""
