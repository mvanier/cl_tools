# `clx` (combinatory logic converter)

This program converts from lambda calculus
to various kinds of combinatory logic bases.

## Usage

```
$ clx -SKI '\fgx . f x (g x)'
S
$ clx -SKIBC '\fgx . f (g x)'
B
$ clx -BCKWI '\fx . f x x'
W
```



