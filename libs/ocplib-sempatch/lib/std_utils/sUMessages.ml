let out_fun =
  try
    ignore @@ Sys.getenv "SEMPATCH_VERBOSE";
    print_string
  with Not_found -> ignore

let debug msg = Printf.ksprintf
    (fun msg -> out_fun ("Debug : " ^ msg))
    msg

let warn msg = Printf.ksprintf
    (fun msg -> out_fun ("Warning : " ^ msg))
    msg
