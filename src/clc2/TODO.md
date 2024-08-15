# TODO for clc2

## Commands

#p K x y;  // print definition of K using x and y

#sl loc;

// Reduce a specific expression, identified by its "location" in the form.
// Locations are indicated by 0-1 strings,
// where 0 means left subexpression in an application,
// and 1 means the right subexpression.

#ccc; // Like #cc, but also print numbers of all subexpressions.

Example:

```
#c;
F F

#s;
--> M (B M M) F

#s;
--> B M M (B M M) F
#ccc;
((((B M) M) ((B M) M)) F)
 0||| |  |  ||| |  |   1
  0|| |  |  1|| |  |
   0| |  1   0| |  1
    0 1       0 1
#sl 1
--> B M M (B M M) (M (B M M))
```

## lambda conversions

TODO

