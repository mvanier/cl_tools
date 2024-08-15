# TODO for clc2

## Commands

```
#sl loc;  // "step at location"

// Reduce a specific expression, identified by its "location" in the form.
// Locations are indicated by 0-1 strings,
// where 0 means left subexpression in an application,
// and 1 means the right subexpression.
// Example:

// F = M (B M M)
B M M (B M M) F;
#ccc;
((((B M) M) ((B M) M)) F)
 0||| |  |  ||| |  |   1
  0|| |  |  1|| |  |
   0| |  1   0| |  1
    0 1       0 1

#sl :1
--> B M M (B M M) (M (B M M))
```

## lambda conversions

TODO

