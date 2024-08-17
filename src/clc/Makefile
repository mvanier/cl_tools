BUILD   = ./_build/default
NAME    = clc
INSTALL = ${HOME}/_/bin
LIB		= clclib.cma
EXT		= exe

default: lang

lib:
	dune build ${LIB}

lang: lib
	dune build main.${EXT}
	cp ${BUILD}/main.${EXT} ${INSTALL}/${NAME}
	chmod 755 ${INSTALL}/${NAME}

conflicts:
	cp ${BUILD}/parser.conflicts .

clean:
	dune clean
	rm -f ${NAME}
	rm -f parser.conflicts
