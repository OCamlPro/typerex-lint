@AddSmallInteger
expressions: e1, e2
when : "is_in_range(e2, -1, 1) && not(is_int_in_range(e2, -1, 1))"
message : "use `succ`, `pred` or nothing instead of adding $e2"
```
add e1 e2
```
