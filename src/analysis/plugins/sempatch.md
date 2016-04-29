@Incr
expressions: e1, e2
message: "Use 'incr $e1' instead of '$e1 := !$e2 + 1.'"
```
e1 := !e2 + 1
```

@Decr
expressions: e1, e2
message:  "Use 'decr $e1' instead of '$e1 := !$e2 - 1'."
```
- e1 := !e2 - 1
+ decr e1
```

@Incr
expressions: e1, e2
message: "Use 'incr $e1' instead of '$e1 := !$e2 + 1.'"
```
e1 := !e2 + 1
```

@Decr
expressions: e1, e2
message:  "Use 'decr $e1' instead of '$e1 := !$e2 - 1'."
```
- e1 := !e2 - 1
+ decr e1
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

@EmptyListComparison
expressions: l
message: "Use a pattern matching instead of comparing to `0`."
```
List.length l = 0
```

@ LetIdentity
expressions: x1, x2, e
```
- let x1 = e in x2
+ e
```
