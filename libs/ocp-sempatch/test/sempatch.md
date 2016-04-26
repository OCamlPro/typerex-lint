# simpleVar
```
- x
+ y
```

# apply
```
- f x
+ foo
```

# functionMatch
```
- fun x -> x
+ foo
```

# letBinding
```
- let x = 1 in x
+ tralala
```

# replaceInsideLet
```
let x = 1 in
- x
+ y
```

# tuples
```
- 1, 2, 3, 4
+ foo
```

# listCompare
variables: l, e1, e2
```
- if List.length l = 0 then e1 else e2
+ match l with
+ | [] -> e1
+ | _  -> e2
```

# function
```
function
  | foo -> true
  | bar ->
-    true
+    false
```
