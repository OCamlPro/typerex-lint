type (+'good, +'bad) t =
  | IsOk of 'good
  | IsError of 'bad

let return x = IsOk x
let fail x = IsError x

let map2 f_ok f_fail = function
  | IsOk x -> IsOk (f_ok x)
  | IsError e -> IsError (f_fail e)

let map f = map2 f SUFun.id
let map_err f x = map2 SUFun.id f x

let bind2 f_ok f_fail = function
  | IsOk x -> f_ok x
  | IsError e -> f_fail e

let bind f = bind2 f fail
let bind_err f x = bind2 return f x

let ok_if cond ok_val err_val =
  if cond then IsOk ok_val else IsError err_val

module Ok_monad_infix =
struct
  let (>>=) x f = bind f x
  let (>|=) x f = map f x
end
module Err_monad_infix =
struct
  let (>>=) x f = bind_err f x
  let (>|=) x f = map_err f x
end
