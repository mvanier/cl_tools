# clc

## Author

Mike Vanier (mvanier@cms.caltech.edu).

## License

GPL version 3. See: https://www.gnu.org/licenses/gpl-3.0.en.html
or the file `LICENSE`.

## Overview

`clc` is a combinatory logic calculator.

Combinator expressions are entered as S-expressions.
You can define new combinators in terms of old ones.
There are commands to reduce combinators in various ways.

## Syntax

* Comments
    ; like this
    ;;| literate comment; gets printed out when evaluated

* Variables: 
    x y z ...  ; must start with a lower-case letter

* Combinators: 
    S K I ...  ; must start with an upper-case letter

* Primitive combinators:
    currently S K I B C W M

* Expressions:
    (S K K x)  ; must be surrounded by parentheses;
               ; application associates to the left,
               ; so this is equivalent to: (((S K) K) x)
    Entering an expression makes it the current expression,
    but does not reduce it.

* Definitions:
    def Q (K I)  ; defines new combinator `Q`

* Commands:
    :q     -- quit  (ctrl-D also works)
    :c     -- print current expression
    :s     -- one reduction step (outermost redex)
    :sc X  -- one reduction step (outermost redex, combinator X only)
    :u     -- undo last step
    :n     -- reduce to normal form
    :ms N  -- set maximum reduction step to N before it's considered
              an infinite loop (default = 25)
    
## Usage

$ clc

  - starts a REPL
  
$ clc <filename>

  - runs clc on a file

## Compiling

This requires OCaml and opam to compile.  The `dune` compilation manager
is needed, so if you don't have it, do

$ opam update
$ opam install dune

Tested with OCaml 4.14.

Go into the `src` directory and type `make` to build and install.
Type `make clean` to clean up compilation artifacts.

You will want to change the install location in the Makefile before compiling.

