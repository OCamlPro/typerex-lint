let flip f x y = f y x
let compose f g x = f (g x)
let compose_binop op f x1 x2 = op (f x1) (f x2)

let ( %> ) = compose

let id x = x
let const x _ = x
