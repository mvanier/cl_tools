.RECIPEPREFIX = >

BUILD   = ./_build/default
NAME    = clc
INSTALL = .
LIB		= clclib.cma
EXT		= exe

default: lang

lib:
> dune build ${LIB}

lang: lib
> dune build main.${EXT}
> cp ${BUILD}/main.${EXT} ${INSTALL}/${NAME}
> chmod 755 ${INSTALL}/${NAME}

clean:
> dune clean
> rm -f ${NAME}

