# patch0
```
f x [@__sempatch_replace y]
```

# patch1
```
x [@__sempatch_replace x+1]
```

# patch2
variables: foo
```
foo [@__sempatch_replace bar]
```

# patch3
variables: y
```
(fun y -> y) [@__sempatch_replace foo]
```


# patch4
variables: x
```
(let x = 1 in x) [@__sempatch_replace tralala]
```
