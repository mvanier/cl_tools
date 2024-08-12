# TODO for clc2

## BUGS

File input isn't working correctly.

Example:

```
// File: test.cl
def B' = S (K S) K;
B' f g x;
#n;
```

Interactively, this gives;

```
B' f g x
--> S (K S) K f g x
--> K S f (K f) g x
--> S (K f) g x
--> K f x (g x)
--> f (g x)
```

But when the input is a file, we get only:

```
B' f g x
--> S (K S) K f g x
```

???

## lambda conversions

TODO

