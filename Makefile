default: exe

all: exe test

SRCDIR=src
TESTDIR=tests

exe:
	(cd $(SRCDIR); $(MAKE) clean depend; $(MAKE) default)

.PHONY: test

clean:
	(cd $(SRCDIR); $(MAKE) clean)

test: exe
	@echo "Testing"
