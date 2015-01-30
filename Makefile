default: exe 

all: exe test

SRCDIR=src
TESTDIR=tests

exe:
	(cd $(SRCDIR); $(MAKE) clean depend default)

.PHONY: test

test: exe
	@echo "Testing"
