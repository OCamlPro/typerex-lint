(*let x = function x -> x +1*)

let neg e = match e with true -> false | false -> true

let neg e = match e with true -> false | false ->true

let rec sort = function 
  | [] -> [] 
  | x :: l -> insert x (sort l) and insert elem = function 
  | [] -> [elem] 
  | x :: l -> if elem < x then elem :: x :: l else x :: insert elem l;;
