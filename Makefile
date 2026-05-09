.POSIX:

EMACS = emacs
EMACSFLAGS = -Q -L . -L tests
ELCS = cook-mode.elc cook-ts-mode.elc tests/cook-ts-mode-test.elc

all: ${ELCS}
clean:
	-rm ${ELCS}
test: all
	${EMACS} ${EMACSFLAGS} -batch -l ert -l cook-ts-mode-test -f ert-run-tests-batch-and-exit

.el.elc:
	${EMACS} ${EMACSFLAGS} -batch -l bytecomp -f batch-byte-compile $<

.PHONY: all clean test
