# Basename of the executable we want to produce
EXE=smtpp
TEST=$(EXE)_test
NAT=$(EXE).opt
BYT=$(EXE).byt
SNAT=static_$(EXE).opt
LIB=lib$(EXE).a

# VERSION=$(shell git log -1 --format="%H (%ai)")
VERSION=smt-2015 20150512
# Load the default OCaml rules from the file Config.mk
# includes default settings
include ../Config.mk

# default target (activate when you invoke only 'make')
default: $(BEST)

include ../Caml.mk

# list of targets that should be recompiled every time
.PHONY: default

PARSER = parser
LEXER = lexer
MLFILES_PRE = \
	version locations ast_utils utils config io \
	sorts theory logic extended_ast checklib \
	pp pre_LA inferlogic
MLFILES_TRANS = satinfo undef_unused pushpop obfuscator
MLFILES_POST = do_parse
MAIN = main
TESTMAIN = test_main
MLIFILES = ast
OTHERFILES = $(PARSER:%=%.mly) $(LEXER=%=%.mll)
SRCMLFILES = $(MLFILES_PRE:%=%.ml) \
	$(MLFILES_TRANS:%=%.ml) \
	$(MLFILES_POST:%=%.ml) \
	$(MLIFILES:%=%.mli) \
	$(MAIN:%=%.ml) \
	$(TESTMAIN:%=%.ml)
SRCFILES = $(SRCMLFILES) $(OTHERFILES)
GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml)
CAMLFILES = $(SRCMLFILES) $(GENFILES)

CORE_CMXFILES = $(MLFILES_PRE:%=%.cmx) \
	$(LEXER:%=%.cmx) \
	$(PARSER:%=%.cmx) \
	$(MLFILES_TRANS:%=%.cmx) \
	$(MLFILES_POST:%=%.cmx)

CORE_CMOFILES = $(MLFILES_PRE:%=%.cmx) \
	$(LEXER:%=%.cmx) \
	$(PARSER:%=%.cmx) \
	$(MLFILES_TRANS:%=%.cmx) \
	$(MLFILES_POST:%=%.cmx)


CMXFILES_LIB =  $(CORE_CMXFILES) smtpplib.cmx
CMOFILES_LIB = $(CMXFILES_LIB:%.cmx=%.cmo)

CMIFILES = $(MLIFILES:%=%.cmi)
CMXFILES = $(CORE_CMXFILES) $(MAIN:%=%.cmx)
CMOFILES = $(CMXFILES:%.cmx=%.cmo)
CMXFILES_TEST = $(CORE_CMXFILES) $(TESTMAIN:%=%.cmx)
CFILES = libwrap
CSRC = $(CFILES:%=%.c)
COBJS = $(CFILES:%=%.o)
MLOBJS = $(CMOFILES:%.cmo=%.o)

LIBS = unix
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)

$(NAT): depend $(CMXFILES)
	$(PP) "LINK  $@"
	$(CAMLBIN) $(CAMLINCLUDES) $(CAMLOPTFLAGS) -o $@ $(OPTLIBDEPS) $(CMXFILES)

$(BYT): depend $(CMIFILES) $(CMOFILES)
	$(PP) "LINK  $@"
	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES)

$(TEST): depend $(CMXFILES_TEST)
	$(PP) "LINK $@"
	$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES_TEST)

static: $(SNAT)
opt: $(NAT)
byt: $(BYT)
tester: $(TEST)

$(SNAT): depend $(CMXFILES)
	$(PP) "STATIC $@"
	$(CAMLBIN) $(CAMLINCLUDES) -noautolink -o $@ $(OPTLIBDEPS) $(CMXFILES) -cclib '-static'

ifeq ($(BEST),opt)
install: $(BEST)
	$(PP) "Install $(NAT) as /usr/local/bin/$(EXE)"
	install $(NAT) /usr/local/bin/$(EXE)
else
install: $(BEST)
	$(PP) "Install $(BYT) as /usr/local/bin/$(EXE)"
	install $(BYT) /usr/local/bin/$(EXE)
endif

clean::
	-$(RM) $(EXE) $(EXE).byt *.annot
	-$(RM) *.a $(PARSER).automaton $(PARSER).conflicts
	for i in $(PARSER) $(LEXER); do\
		$(RM) -f $$i.ml $$i.mli; \
	done

lib: $(CMOFILES_LIB) $(CMXFILES_LIB) $(MLOBJS) $(COBJS)
	ocamlmklib -verbose -o smtpp $(CMOFILES_LIB) $(MLOBJS) $(COBJS)
#	ocamlmklib -verbose -o smtpp $(CMXFILES_LIB)
#	ocamlmklib -verbose -o smtpp $(COBJS)
# ocamlmklib -verbose -custom -o smtpp $(CMOFILES_LIB) $(CMXFILES_LIB) $(COBJS)

.PHONY: beforedepend depend version.ml

# There are generated files so we might need to do depend twice
# beforedepend serves exactly that purpose
version.ml:
	echo 'let version = "$(VERSION)"' > version.ml

beforedepend: $(CAMLFILES)
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

depend:: beforedepend $(CAMLFILES)

.PHONY: license

ifneq ($(HEADACHE),no)
license:
	@for i in $(SRCFILES); do \
		$(HEADACHE) -c ../misc/headache_config.txt -r -h ../LICENSE $$i; \
	done
endif

ifneq ($(CAMLDOC),no)
doc: $(SRCMLFILES)
	$(CAMLDOC) -html -d ../doc/html $(SRCMLFILES)
endif

.PHONY: stats
stats:
	@wc -l $(SRCFILES)
