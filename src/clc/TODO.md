# TODO for clc2

## Archive the old CLC, make clc2 the definitive one.

And rename it to just `clc`.

## More TMAM exercises!

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

Add a :skibcw converter!

## Define current expression.

Add this form:

```
#def F x;
```

which will define `[F x]` to be the current expression.

## Substitutions

We need a command to do user-specified substitutions e.g.

```
#subst :01010 x;  // location, substitution
```

This is simple to implement.
However, the onus is on the user to verify that the substitution is valid.
For instance,

```
#subst :01010 x;
```

would be valid if the subexpression at location :01010 is [B x I], since:

```
>> B x I y;
>> #n;
B x I y
--> x (I y)
--> x y
```

So [B x I] is the same as [x].
In general, proving equivalence of combinatory terms is IIRC undecidable,
so this can't be fully automated.

## Converter test cases:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
#convert :skibc \f . ((\x . (f (x x))) (\x . (f (x x))));
```

## Other optimizations?

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

Also see Oleg's paper.
