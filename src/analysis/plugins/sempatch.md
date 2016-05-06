@Incr
expressions: e1, e2
message: "Use 'incr $e1' instead of '$e1 := !$e2 + 1.'"
when: "e1 = e2"
```
e1 := !e2 + 1
```

@Decr
expressions: e1, e2
message:  "Use 'decr $e1' instead of '$e1 := !$e2 - 1'."
when: "e1 = e2"
```
- e1 := !e2 - 1
+ decr e1
```

@AddSmallInteger
expressions: e1, e2
when : "is_in_range(e2, -1, 1) && not(is_int_in_range(e2, -1, 1))"
message : "use `succ`, `pred` or nothing instead of adding $e2"
```
add e1 e2
```

@CompToFalse
expressions: cond
message: "Use 'not $cond' instead of '$cond = false'."
```
- cond = false
+ not cond
```

@CompToFalse2
expressions: cond
message: "Use '$cond' instead of '$cond != false'."
```
- cond != false 
+ cond 
```

@ CompToFalse3
expressions: cond
message: "Use 'not $cond' instead of '$cond == false'."
```
- cond == false
+ not cond
```

@ CompToTrue1
expressions: cond
message: "Use '$cond' instead of '$cond = true'."
```
- cond = true
+ cond
```

@ CompToTrue2
expressions: cond
message: "Use '$cond' instead of '$cond == true'."
```
- cond == true
+ cond
```

@CompToTrue3
expressions: cond
message: "Use 'not $cond' instead of '$cond <> true'."
```
- cond <> true
+ not cond
```

@ EmptyListComparison
expressions: l, i
message: "Use a pattern matching instead of comparing to `$i`."
when : "is_int_in_range(i, 0, 5)"
```
List.length l = i
```

@ ConstantIf
expressions: cond, e1, e2
message: "Constant if-then-else: there is no need to use a if-then-else."
when: "e1 = e2"
```
if cond then e1 else e2
```

@ UselessIf
expressions: cond
message: "Useless if-then-else: use '$cond' instead of 'if $cond then true else false'."
```
- if cond then true else false
+ cond
```

@ LetIdentity
expressions: x1, x2, e
message: "Useless let-binding: use '$e' without let-binding."
when: "x1 = x2"
```
- let x1 = e in x2
+ e
```

