# TODO for clc2

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

TODO: SKIBC, BCKW

Test case:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
```

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

### Other useful features

* Make the converted lambda expression the current expression.

* Allow "appending" to the current expression:

```
>> S;
>> #append K K x;
>> #n;
--> K x (K x)
--> x
```

## Archive the old CLC, make clc2 the definitive one.

Also merge this branch with the main branch.

