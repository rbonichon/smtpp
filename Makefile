default: exe

all: static exe doc test

SRCDIR=src
TESTDIR=tests

exe:
	(cd $(SRCDIR); $(MAKE) clean depend; $(MAKE) default)

.PHONY: test

clean:
	(cd $(SRCDIR); $(MAKE) clean)

test: exe
	@echo "Testing"

doc:
	(cd $(SRCDIR); $(MAKE) doc)

static:
	(cd $(SRCDIR); $(MAKE) static)
