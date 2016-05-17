let _ = ignore (let add = Int32.add in add 1l 1l)
let _ = ignore (let add = Int64.add in add 1L 1L)
let _ = ignore (let add = Nativeint.add in add 0n 1n)
let _ = let add = (+) in ignore (add 0 1)
