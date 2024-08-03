BUILD   = ./_build/default
NAME    = clc2
INSTALL = ${HOME}/_/bin
LIB		= clc2lib.cma
EXT		= exe

#default: lang
default: lib

lib:
	dune build ${LIB}

lang: lib
	dune build main.${EXT}
	cp ${BUILD}/main.${EXT} ${INSTALL}/${NAME}
	chmod 755 ${INSTALL}/${NAME}

clean:
	dune clean
	rm -f ${NAME}
