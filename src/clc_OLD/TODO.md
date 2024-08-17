# TODO

## Syntax

### Implementation

Change all the syntax so that `;` ends the input.
Use `//` for comments and `///` for literate comments.
Write the parser in Menhir.

Note that this has the undesirable effect of converting

```
:n
```

to:

```
:n;
```

However, I think I can live with this.
Perhaps this could be changed to:

```
#n;
```

in analogy to OCaml's `#use` form and similar REPL forms.

### Definitions

Improve the definition syntax so I can write e.g.

```
def F = (K I)
def S (f g x) = (f x (g x))
def X (x) = (x K S K)  ; universal combinator
```

## Commands

Commands to implement:

:scn S 3  ; reduce third S, going depth-first

## Other

Add Y as a primitive combinator?
Or as a built-in combinator?

