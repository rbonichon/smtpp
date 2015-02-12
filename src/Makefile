# Basename of the executable we want to produce
EXE=smtpp
NAT=$(EXE).opt
BYT=$(EXE).byt
SNAT=static_$(EXE).opt

# Load the default OCaml rules from the file Config.mk
# includes default settings
include ../Config.mk

# default target (activate when you invoke only 'make')
default: $(BEST)

# list of targets that should be recompiled every time
.PHONY: default

PARSER = parser
LEXER = lexer
MLFILES_PRE = config utils locations io pp
MLFILES_TRANS = pushpop obfuscator
MLFILES_POST = do_parse main
MLIFILES = ast
OTHERFILES = $(PARSER:%=%.mly) $(LEXER=%=%.mll)
SRCMLFILES = $(MLFILES_PRE:%=%.ml) \
	$(MLFILES_TRANS:%=%.ml) \
	$(MLFILES_POST:%=%.ml) \
	$(MLIFILES:%=%.mli)
SRCFILES = $(SRCMLFILES) $(OTHERFILES)
GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml)
CAMLFILES = $(SRCMLFILES) $(GENFILES)
CMXFILES = $(MLFILES_PRE:%=%.cmx) \
	$(LEXER:%=%.cmx) \
	$(PARSER:%=%.cmx) \
	$(MLFILES_TRANS:%=%.cmx) \
	$(MLFILES_POST:%=%.cmx)
CMIFILES = $(MLIFILES:%=%.cmi)
CMOFILES = $(CMXFILES:%.cmx=%.cmo)

LIBS =
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)

$(NAT): depend $(CMXFILES)
	@echo "EXE $@"
	@$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES)


$(BYT): depend $(CMIFILES) $(CMOFILES)
	@echo "EXE $@"
	@$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES)

static: $(SNAT)
opt: $(NAT)
byt: $(BYT)

$(SNAT): depend $(CMXFILES)
	@echo "STATIC $@"
	$(CAMLBIN) $(CAMLINCLUDES) -noautolink -o $@ $(OPTLIBDEPS) $(CMXFILES) -cclib '-static'

clean::
	-$(RM) $(EXE) $(EXE).byt *.annot
	for i in $(PARSER) $(LEXER); do\
		$(RM) -f $$i.ml $$i.mli; \
	done

.PHONY: beforedepend depend

# There are genereated files so we might need to do depend twice
# beforedepend serves exactly that purpose
beforedepend: $(CAMLFILES)
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

depend:: beforedepend $(CAMLFILES)

.PHONY: license

ifneq ($(HEADACHE),no)
license:
	@echo "Adding license prelude to files"
	@for i in $(SRCFILES); do \
		headache -r -h ../LICENSE $$i; \
	done
endif

ifneq ($(CAMLDOC),no)
doc: $(SRCMLFILES)
	$(CAMLDOC) -html -d ../doc $(SRCMLFILES)
endif

.PHONY: stats
stats:
	@wc -l $(SRCFILES)
