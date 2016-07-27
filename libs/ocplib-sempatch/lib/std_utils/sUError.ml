type (+'good, +'bad) t =
  | Ok of 'good
  | Error of 'bad

let return x = Ok x
let fail x = Error x

let map2 f_ok f_fail = function
  | Ok x -> Ok (f_ok x)
  | Error e -> Error (f_fail e)

let map f = map2 f SUFun.id
let map_err f x = map2 SUFun.id f x

let bind2 f_ok f_fail = function
  | Ok x -> f_ok x
  | Error e -> f_fail e

let bind f = bind2 f fail
let bind_err f x = bind2 return f x

let ok_if cond ok_val err_val =
  if cond then Ok ok_val else Error err_val

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
