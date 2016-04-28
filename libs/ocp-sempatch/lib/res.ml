open Std_utils

type 'a t = ('a, 'a) result

let return x = Ok x
let fail x = Error x

let map f = function
  | Ok x -> Ok (f x)
  | Error x -> Error (f x)

let unwrap = function
  | Ok x -> x
  | Error x -> x

let make_error x = Error (unwrap x)
let make_ok x = Ok (unwrap x)

let bind f = function
  | Ok x -> f x
  | Error x -> make_error (f x)

let bind_err f = function
  | Ok x -> make_ok (f x)
  | Error x -> f x

module Ok_monad_infix =
struct
  let (>>=) x f = bind f x
  let (>|=) x f = map f x
end

module Err_monad_infix =
struct
  let (>>=) x f = bind_err f x
  let (>|=) x f = map f x
end
