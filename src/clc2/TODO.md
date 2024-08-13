# TODO for clc2

## Commands

#p K x y;  // print definition of K using x and y

#sn N;     // Do N steps.

#sl loc;

// Reduce a specific expression, identified by its "location" in the form.
// Locations are indicated by 0-1 strings,
// where 0 means left subexpression in an application,
// and 1 means the right subexpression.

#cc;  // Print out current expr in fully-parenthesized form.
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
--> ((((B M) M) ((B M) M)) F)
  0 = (((B M) M) ((B M) M))
  1 = F
  00 = ((B M) M)
  01 = ((B M) M)
  000 = (B M)
  001 = M
  010 = (B M)
  011 = M
  0000 = B
  0001 = M
  0100 = B
  0101 = M
#sl 1
--> B M M (B M M) (M (B M M))
```

## lambda conversions

TODO

