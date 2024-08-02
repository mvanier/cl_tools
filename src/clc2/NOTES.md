# clc2 (THIS BRANCH)

This is a complete refactor of clc.

## Goals

* Combinators defined in the language

* Nicer syntax with Menhir parser

* Extra utilities _e.g._
  conversions from lambda calculus in the language

```
def I x = x ;
def K x y = x ;
def B f g x = f (g x) ;
def C f x y = f y x ;
def W f x = f x x ;
def S f g x = f x (g x) ;
def X x = x K S K ;
def Y x = x S K ;
S K K x ;
#n ;
--> x
#c ski \fgx . f x (g x);
--> S
```

