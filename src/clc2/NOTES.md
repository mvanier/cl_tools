# clc2 (THIS BRANCH)

This is a complete refactor of clc.

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

## Evaluation

### Definitions

1. Convert to AST
2. Expand to IR
3. Convert to number representation:
   def S x y z = x z (y z);
   -->
   def S = [3, (0 2 (1 2))];

### Top-level expressions

1. Reduce outermost redex
2. Continue until a normal form is reached
3. Stop after N steps if no normal form is reached
(N can be set).

## Commands

```
// Set maximum number of reduction steps.
#max-steps 1000 ;

// Set working expression.
S K K x ;

// Print current expression.
#c;
--> S K K x

// Print current expression, with all applications explicit.
#cc;
--> (((S K) K) x)

// Normalize working expression.
#n;

// Make one evaluation step of working expression
#s ;
--> K x (K x)

// Make N steps
#s <n>;

// Convert lambda expressions to combinators.
#convert ski : \fgx . f x (g x) ;
--> S
```

