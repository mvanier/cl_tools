# TODO for clc2

## Documentation

I need a definitive list of commands and syntax.

## lambda conversions

TODO: SKIBC, BCKW

Test case:

```
#convert :ski \f . ((\x . (f (x x))) (\x . (f (x x))));
```

NOTE: The SKI algorithm I've implemented is not a good one.
The resulting expression are correct, but excessively long.
Example:

```
>> #convert :ski \f g x . ((f x) (g x));
--> S (S (K S) (S (K K) (S (K S) (S (S (K S) (S (K K) I)) (K I))))) (K (S (S (K S) (S (K K) I)) (K I)))
```

when it should be just:

```
>> #convert :ski \f g x . ((f x) (g x));
--> S
```

Try implementing some optimizations as described in:

https://en.wikipedia.org/wiki/Combinatory_logic

Note especially the eta-reduction rule.

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

