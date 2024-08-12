# TODO for clc2

## evaluator

Still some bugs!

Example:

```
K a b --> a
K a b c --> a c
K a b c d --> nothing!
```

SUBTLE PROBLEM:

- we left-flatten the form
- we can only reduce a _prefix_ of the left-flattened form!

Is this a good way to evaluate forms?  Probably not.

## Basis

TODO

## main

Might want the ability to read in a file.
