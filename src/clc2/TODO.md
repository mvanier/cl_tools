# TODO for clc2

## Archive the old CLC, make clc2 the definitive one.

And rename it to just `clc`.

## More TMAM exercises!

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

Add a :skibcw converter!

Some converters (BCKW, BCKWI) are very inefficient.
What would improve things a lot is if there was a table of equivalences!
Examples:

```
S K K == I
B I = I
B x I == x
```

Need a new command: #simplify :ski (:bcwki etc.)
You could even specify _which_ simplifications to do,
even unknown ones!

```
#simplify :bcwki (B x I) x;
```

First, it would check that (B x I) is equivalent to (x).
Then it would apply the simplification.

Doing these simplifications could be tricky, though.

## Converter test cases:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
#convert :skibc \f . ((\x . (f (x x))) (\x . (f (x x))));
```

## Other optimizations?

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

Also see Oleg's paper.

