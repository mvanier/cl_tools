# TODO for clc2

## Intermediate representations

Need IR2, which converts applications to a numeric form.
It's a bit like de Bruijn indices, but it's not that.

```
def S x y z = (x z (y z));
-->
def S x y z = ((x z) (y z));
-->
def S = [3, [[0 2] [1 2]]]
```
