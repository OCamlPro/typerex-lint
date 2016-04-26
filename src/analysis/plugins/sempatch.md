# List function on singleton String.concat
variables: s, l, x
```
- String.concat s [x]
```
# List function on singleton List.map
variables: f, l, x
```
- List.map f [x]
```

# List function on singleton List.fold_left
variables: f, acc, l, x
```
- List.fold_left f acc [x]
```

# List function on singleton List.fold_right
variables: f, acc, l, x
```
- List.fold_right f acc [x]
```

# Useless if
variables: cond
```
- if cond then true else false
```

# Backwards if
variables: cond, expr
```
- if cond then () else expr
```

# Useless else
variables: cond, expr
```
- if cond then expr else ()
```

# Inlined function Str.first_chars
variables:s
integer:n
```
- String.sub s 0 n
```

# Inlined function Str.string_after
variables:s, s'
integer:n, n'
when: equal(s, s') && equal(n, n')
```
- String.sub s 0 (String.length s' - n')
```

# Inlined function Str.last_chars
variables:s, s'
integer:n, n'
when: equal(s, s') && equal(n, n')
```
- String.sub s (String.length s' - n) n'
```

# Empty list test <>
variables: l
```
- List.length l > 0
```

# Empty list test =
variables: l
```
- List.length l = 0
```

# Comparison to boolean = true
variables: cond
```
cond = true
```

# Comparison to boolean == true
variables: cond
```
cond == true
```
# Comparison to boolean <> true
variables: cond
```
cond <> true
```

# Comparison to boolean = false
variables: cond
```
cond = false
```

# Comparison to boolean == true
variables: cond
```
cond == false
```

# Comparison to boolean != false
variables: cond
```
cond != false
```


