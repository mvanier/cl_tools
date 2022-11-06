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

* Comments start with a semicolon and go to the end of the line ; like this.
A literate comment starts with `;;|` and gets printed out when "evaluated".

* Variables: start with a lowercase letter, like this: `x y z`. 

* Combinators: start with an uppercase letter, like this: `S K I`.

* Primitive combinators: currently include: `S K I B C W M`.

* Expressions must be surrounded by parentheses, like this: `(S K K x)`.
  Function application associates to the left, so this is equivalent to
  `(((S K) K) x)`. Entering an expression makes it the current expression,
  but does not reduce it.

* Definitions use the `def` keyword: `def Q (K I)` defines a new combinator
  called `Q`.

* Commands start with a colon (`:`) and include:
  * `:q`      quit  (ctrl-D also works)
  * `:c`      print current expression
  * `:s`      one reduction step (outermost redex)
  * `:sc X`   one reduction step (outermost redex, combinator X only)
  * `:u`      undo last step
  * `:n`      reduce to normal form
  * `:ms N`   set maximum reduction step to N before it's considered
              an infinite loop (default = 25)

## Literate syntax

Normal clc filenames end in `.cl`.
Literate filenames end in `.lc`.

A literate file is the same as a regular clc file except:

* Lines that are not literate comments start with "> ";
  the "> " is discarded and the rest of the line is processed
  as usual.

* All other lines have a ";;| " prepended to them, so they are all
  literate comments.
    
## Usage

~~~
$ clc
  ; starts a REPL
  
$ clc <filename>
  ; runs clc on a file
  ; filename should end in `.cl`;
  ; if filename ends in `.lc`, use literate syntax
~~~

## Compiling

This requires OCaml and opam to compile.  The `dune` compilation manager
is needed, so if you don't have it, do

~~~
$ opam update
$ opam install dune
~~~

Tested with OCaml 4.14.

Go into the `src` directory and type `make` to build and install.
Type `make clean` to clean up compilation artifacts.

You will want to change the install location in the Makefile before compiling.

