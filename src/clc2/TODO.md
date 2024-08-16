# TODO for clc2

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

Some converters are very inefficient.
What would improve things a lot is if there was a table of equivalences!
Examples:

```
S K K == I
B x I == x
```

Doing these simplifications could be tricky, though.

Test cases:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
#convert :skibc \f . ((\x . (f (x x))) (\x . (f (x x))));
```

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

## Archive the old CLC, make clc2 the definitive one.

Also merge this branch with the main branch.

