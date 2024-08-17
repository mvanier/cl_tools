# clc

## Goals

* Combinators defined in the language

* Nicer syntax with Menhir parser

* Extra utilities _e.g._
  conversions from lambda calculus in the language

```
// Definitions.
def I x = x ;
def K x y = x ;
def B f g x = f (g x) ;
def C f x y = f y x ;
def W f x = f x x) ;
def S f g x = f x (g x) ;
def X x = x K S K ;
def Y x = x S K ;
def Q = K I ;

// Enter a working expression.
S K K x ;

// Normalize the working expression.
#n ;
--> K x (K x)
--> x

// Single-stepping.
S K K x ;
#s ;
--> K x (K x)
#s ;
--> x

// Make working expression and normalize.
#n  S K K x ;
--> K x (K x)
--> x

// Convert lambda calculus to SKI combinators.
#c ski : \fgx . f x (g x) ;
--> S

// Change parameters.
#maxsteps 1000 ;   // maximum normalization steps before stopping.
```

## Commands

```
// Set maximum number of reduction steps.
#max-steps 1000;

// Set display mode.
#display-raw;     // explicitly shows all applications
#display-normal;  // uses usual display conventions

// Set working expression.
S K K x;

// Print current expression.
#c;
--> S K K x

// Print current expression, with all applications explicit.
#cc;
--> (((S K) K) x)

// Like #cc, but also print numbers of all subexpressions.
B M M (B M M) F;
#ccc;
((((B M) M) ((B M) M)) F)
 0||| |  |  ||| |  |   1
  0|| |  |  1|| |  |
   0| |  1   0| |  1
    0 1       0 1

// Normalize working expression.
#n;

// Make one evaluation step of working expression
#s;
--> K x (K x)

// Make N steps.
#sn 3;

// Make one step for a location indicated by 0-1 strings
// (0 = left subexpression, 1 = right).
#sl :101;  (* right, left, right *)

// Convert lambda expressions to combinators.
#convert :ski \f g x . f x (g x);
--> S

// Append one or more expressions to the current expression.
#append K x y;
```

## Interesting factoids

### Y combinator

There are a number of very simple implementations of the Y combinator.
These involve the Lark (L) combinator.

```
def Y = B M L;
def Y = B (L I) L;
def Y = S L L;
```

The Lark is not that easy to implement in terms of other combinators.
The simplest defintion I've found is [L = C B M].

