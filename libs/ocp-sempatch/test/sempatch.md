@simpleVar
(* This is a comment *)
```
- x
(* This is a comment too *)
+ y
```

@apply
```
- f x
+ foo
```

(* And this is also a commment *)

@functionMatch
```
- fun x -> x
+ foo
```

@letBinding
```
- let x = 1 in x
+ tralala
```

@replaceInsideLet
```
let x = 1 in
- x
+ y
```

@tuples
```
- 1, 2, 3, 4
+ foo
```

@listCompare
expressions: l, e1, e2
message: "This is a message"
```
- if List.length l = 0 then e1 else e2
+ match l with
+ | [] -> e1
+ | _  -> e2
```

@function
```
function
  | foo -> true
  | bar ->
-    true
+    false
```

@match
```
- match x with
-   | foo -> true
-   | bar -> false
+ x = foo
```
