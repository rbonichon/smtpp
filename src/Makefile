# Basename of the executable we want to produce
EXE=smtpp
NAT=$(EXE).opt
BYT=$(EXE).byt
SNAT=static_$(EXE).opt
LIB=lib$(EXE).a

VERSION=$(shell git log -1 --format="%H (%ai)")
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
	locations ast_utils utils config io \
	sorts theory logic extended_ast pp checks
MLFILES_TRANS = preprocessor undef_unused pushpop obfuscator
MLFILES_POST = do_parse
MAIN = main
MLIFILES = ast
OTHERFILES = $(PARSER:%=%.mly) $(LEXER=%=%.mll)
SRCMLFILES = $(MLFILES_PRE:%=%.ml) \
	$(MLFILES_TRANS:%=%.ml) \
	$(MLFILES_POST:%=%.ml) \
	$(MLIFILES:%=%.mli)
SRCFILES = $(SRCMLFILES) $(OTHERFILES)
GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml)
CAMLFILES = $(SRCMLFILES) $(GENFILES)

CMXFILES_LIB = $(MLFILES_PRE:%=%.cmx) \
	$(LEXER:%=%.cmx) \
	$(PARSER:%=%.cmx) \
	$(MLFILES_TRANS:%=%.cmx) \
	$(MLFILES_POST:%=%.cmx) \
	smtpplib.cmx
CMOFILES_LIB = $(CMXFILES_LIB:%.cmx=%.cmo)

CMIFILES = $(MLIFILES:%=%.cmi)
CMXFILES = $(CMXFILES_LIB) $(MAIN:%=%.cmx)
CMOFILES = $(CMXFILES:%.cmx=%.cmo)
CFILES = libwrap
CSRC = $(CFILES:%=%.c)
COBJS = $(CFILES:%=%.o)
MLOBJS = $(CMOFILES:%.cmo=%.o)

LIBS =
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)

$(NAT): depend $(CMXFILES)
	$(PP) "LINK  $@"
	$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES)


$(BYT): depend $(CMIFILES) $(CMOFILES)
	$(PP) "LINK  $@"
	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES)

static: $(SNAT)
opt: $(NAT)
byt: $(BYT)

$(SNAT): depend $(CMXFILES)
	$(PP) "STATIC $@"
	$(CAMLBIN) $(CAMLINCLUDES) -noautolink -o $@ $(OPTLIBDEPS) $(CMXFILES) -cclib '-static'
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

.PHONY: beforedepend depend

# There are genereated files so we might need to do depend twice
# beforedepend serves exactly that purpose
beforedepend: $(CAMLFILES)
	sed -i -e 's/let version = ".*"/let version = "$(VERSION)"/g' config.ml
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

depend:: beforedepend $(CAMLFILES)

.PHONY: license

ifneq ($(HEADACHE),no)
license:
	@for i in $(SRCFILES); do \
		headache -r -h ../LICENSE $$i; \
	done
endif

ifneq ($(CAMLDOC),no)
doc: $(SRCMLFILES)
	$(CAMLDOC) -html -d ../doc/html $(SRCMLFILES)
endif

.PHONY: stats
stats:
	@wc -l $(SRCFILES)
