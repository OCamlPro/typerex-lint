@Incr
expressions: e1, e2
message: "Use 'incr $e1' instead of '$e1 := !$e2 + 1.'"
when: "e1 = e2"
severity: 1
```
e1 := !e2 + 1
```

@Decr
expressions: e1, e2
message:  "Use 'decr $e1' instead of '$e1 := !$e2 - 1'."
when: "e1 = e2"
severity: 1
```
- e1 := !e2 - 1
+ decr e1
```
@CompToFalse
expressions: cond
message: "Use 'not $cond' instead of '$cond = false'."
severity: 6
```
- cond = false
+ not cond
```

@CompToFalse2
expressions: cond
message: "Use '$cond' instead of '$cond != false'."
severity: 6
```
- cond != false
+ cond
```

@ CompToFalse3
expressions: cond
message: "Use 'not $cond' instead of '$cond == false'."
severity: 6
```
- cond == false
+ not cond
```

@ CompToTrue1
expressions: cond
message: "Use '$cond' instead of '$cond = true'."
severity: 6
```
- cond = true
+ cond
```

@ CompToTrue2
expressions: cond
message: "Use '$cond' instead of '$cond == true'."
severity: 6
```
- cond == true
+ cond
```

@CompToTrue3
expressions: cond
message: "Use 'not $cond' instead of '$cond <> true'."
severity: 6
```
- cond <> true
+ not cond
```

@ EmptyListComparison
expressions: l, i
message: "Use a pattern matching instead of comparing to `$i`."
when : "is_int_in_range(i, 0, 5)"
severity: 10
```
List.length l = i
```

@ UselessIf
expressions: cond
message: "Useless if-then-else: use '$cond' instead of 'if $cond then true else false'."
severity: 7
```
- if cond then true else false
+ cond
```

@ UselessElse
expressions: cond, e
message: "Useless else: use 'if $cond then $e' instead of 'if $cond then $e else ()'."
severity: 2
```
- if cond then e else ()
+ if cond then e
```

@ BackwardIf
expressions: cond, e
message: "Backward if: use 'if not $cond then $e' then $e' instead of 'if $cond then () else $e'."
severity: 2
```
- if cond then () else e
+ if not cond then e
```

@ LetIdentity
expressions: x1, x2, e
message: "Useless let-binding: use '$e' without let-binding."
when: "x1 = x2"
severity: 2
```
- let x1 = e in x2
+ e
```

@ ListOpOnLit
expressions: expr, list
message: "List operation on singleton: use '$expr :: $list' instead of '[$expr] @ $list'."
severity: 6
```
- [ expr ] @ list
+ expr :: list
```

@ ListOpOnLit_2
expressions: list
message: "List operation on empty list: use '$list' instead of '[] @ $list'."
severity: 6
```
- [] @ list
+ list
```

@ ListOpOnLit_3
expressions: list
message: "List operation on empty list: use '$list' instead of '$list @ []'."
severity: 6
```
- list @ []
+ list
```

@ ConstantIf
expressions: cond, e1, e2
message: "Constant if-then-else: there is no need to use a if-then-else."
when: "e1 = e2"
severity: 5
```
if cond then e1 else e2
```

@ Discarded_result
message : "Avoid discarding the result of expressions"
expressions: e1, e2
severity: 6
```
let _ = e1 in e2
```

@ Catch_Sys_Break
message: "Do not try to catch the Sys.Break exception"
expressions: e1, e2
severity: 10
```
try e1 with Sys.Break -> e2
```

@ Identity_sprintf
message: "Useless sprintf, use '$e' directly"
expressions: e
when: "is_string_lit(e)"
severity : 5
```
Printf.sprintf e
```

@ Identity_sprintf_2
message: "Useless sprintf, use '$e' directly"
expressions: e
severity : 5
```
Printf.sprintf "%s" e
```

@ Dynlink
message: "Do not use dynamic linking"
severity: 8
```
Dynlink.loadfile
```

@ Dynlink_2
message: "Do not use dynamic linking"
severity: 8
```
Dynlink.loadfile_private
```
@ String_Create
message: "Do not use String.create"
severity: 1
```
- String.create
+ String.make
```

@ Catch_Wildcard
message: "Do not try to catch the exceptions with '_'"
expressions: e1, e2
severity: 1
```
try e1 with _ -> e2
```
