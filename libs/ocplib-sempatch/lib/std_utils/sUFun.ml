let flip f x y = f y x
let compose f g x = f (g x)

let ( %> ) = compose

let id x = x
let const x _ = x
