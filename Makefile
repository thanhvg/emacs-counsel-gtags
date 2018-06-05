emacs ?= emacs
#BEMACS = $(emacs) -batch -l targets/elpa.el
BEMACS = $(emacs) -batch

all: test

# Use LC_ALL=C to avoid locale dependencies in the dates!
test: clean
	LC_ALL=C $(BEMACS) -l test/setup-unit-tests.el -l test/straight.el/straight.el -l test/unit-tests.el -l counsel-gtags.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile counsel-gtags.el

clean:
	rm -f f.elc

.PHONY:	all test
