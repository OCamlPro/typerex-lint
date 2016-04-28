let test1 = let f = (+) 1 in assert (f = 2) (* Transforms the variable `f` into the function call `f 1` *)

let test2 = let x = 1 in assert (y = 1) (* Rename `y` into `x` *)

let test3 = let x = 0 in let f = x + 1 in ignore x; assert (f = 3) (* Transforms the int f into a function int -> int and the variable f into `f 2` *)

let test4 =
  let module Foo =
  struct
    (* Adds an "open List" directive *)
    let l = [1; 2] in assert (map ((+) 1) l = [2; 3]);;
  end
  in ()

let not_test_5 = let y=1 in assert (y=1)
let test5 =
  let y = 1 in assert (y=1);
  let x = 1 in assert (y=1)

let test6 =
  let x = 1 in
  (let foo = () in ignore foo; assert (y=1));
  (let bar = () in ignore bar; assert (y=1));
  (let not_matched = () in let y=2 in ignore not_matched, assert (y=2))

let test7 =
  let x = 1 in
  assert (y + (let y=2 in y) = 3)
