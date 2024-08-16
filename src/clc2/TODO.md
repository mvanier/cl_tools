# TODO for clc2

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

Currently broken at the parser level!
I need to be able to handle recursive lambda expressions.
The most obvious one is the (normal order) Y combinator:

```
\f . (\x . f (x x)) (\x . f (x x))
```

## Archive the old CLC, make clc2 the definitive one.

Also merge this branch with the main branch.

