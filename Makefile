.POSIX:

EMACS = emacs
EMACSFLAGS = -Q -L .
ELCS = cook-mode.elc cook-ts-mode.elc

all: ${ELCS}
clean:
	-rm ${ELCS}

.el.elc:
	${EMACS} ${EMACSFLAGS} -batch -l bytecomp -f batch-byte-compile $<

.PHONY: all clean
