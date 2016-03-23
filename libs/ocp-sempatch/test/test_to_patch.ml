let test1 = let f = (+) 1 in assert (f = 2) (* Transforms the variable `f` into the function call `f 1` *)

let test2 = let x = 1 in assert (y = 1) (* Rename `y` into `x` *)

let test3 = let x = 0 in let f = x + 1 in assert (f = 3) (* Transforms the int f into a function int -> int and the variable f into `f 2` *)

let test4 =
  let module Foo =
  struct
    (* Adds an "open List" directive *)
    let l = [1; 2] in assert (map ((+) 1) l = [2; 3]);;
  end
  in ()
