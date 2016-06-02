
let test_empty_list_cmp list texpr fexpr =
  (* Matched by @EmptyListComparison *)
  if List.length list = 0 then texpr
  else fexpr;
  if List.length list = 1 then texpr
  else fexpr;
  if List.length list = 2 then texpr
  else fexpr;
  if List.length list = 3 then texpr
  else fexpr;
  if List.length list = 4 then texpr
  else fexpr;
  if List.length list = 5 then texpr
  else fexpr;
  (* Not matched *)
  if List.length list = 6 then texpr
  else fexpr;
