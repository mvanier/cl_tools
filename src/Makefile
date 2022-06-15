.RECIPEPREFIX = >

BUILD   = ./_build/default
NAME    = clc
INSTALL = ${HOME}/_/bin
LIB		= clclib.cma
EXT		= exe

default: lang

lib:
> dune build ${LIB}

lang: lib
> dune build main.${EXT}
> cp ${BUILD}/main.${EXT} ${INSTALL}/${NAME}
> chmod 755 ${INSTALL}/${NAME}

lexer_deps:  lib lexer.mll lexer.mli
parser_deps: lexer_deps parser.ml parser.mli
repl_deps:   parser_deps repl.ml repl.mli

lexer_test: lexer_deps lexer_test.ml
> rm -f ./lexer_test
> dune build lexer_test.${EXT}
> cp ${BUILD}/lexer_test.${EXT} lexer_test

parser_test: parser_deps parser_test.ml
> rm -f ./parser_test
> dune build parser_test.${EXT}
> cp ${BUILD}/parser_test.${EXT} parser_test

test: lexer_test parser_test

clean:
> dune clean
> rm -f ${NAME}

realclean: clean
> rm -f lexer_test parser_test
