default: exe

include ./Config.mk

all: exe static doc test

SRCDIR=src
TESTDIR=tests

exe:
	$(PP) "Executable ..."
	$(QUIET_MAKE) -C $(SRCDIR) clean depend;
	$(QUIET_MAKE) -C $(SRCDIR) default;

.PHONY: test

clean::
	(cd $(SRCDIR); $(MAKE) clean)

test: exe
	$(PP) "Testing"

doc:
	$(QUIET_MAKE) -C doc

static:
	$(PP) "Static binary ..."
	$(QUIET_MAKE) -C $(SRCDIR) static;

install:
	$(QUIET_MAKE) -C $(SRCDIR) install;
