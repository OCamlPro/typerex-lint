
let subsitute str substs =
  let rec loop str = function
    | [] -> str
    | (pattern, replace) :: others ->
      let str =
        Str.global_substitute (Str.regexp pattern) (fun _ -> replace) str in
      loop str others in
  loop str substs
