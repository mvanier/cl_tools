# TODO for clc2

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

TODO: BCKW

Test cases:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
#convert :skibc \f . ((\x . (f (x x))) (\x . (f (x x))));
```

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

## Archive the old CLC, make clc2 the definitive one.

Also merge this branch with the main branch.

