# Basename of the executable we want to produce
EXE=smtpp
NAT=$(EXE).opt
BYT=$(EXE).byt

# default target (activate when you invoke only 'make')
default: $(NAT) $(BYT)

# list of targets that should be recompiled every time
.PHONY: default

# Load the default OCaml rules from the file Caml.mk
include ../Caml.mk

PARSER = parser
LEXER = lexer
MLFILES_PRE = config utils locations io pp
MLFILES_TRANS = pushpop obfuscator
MLFILES_POST = main
MLIFILES = ast
SRCMLFILES= $(MLFILES_PRE:%=%.ml) \
	$(MLFILES_TRANS:%=%.ml) \
	$(MLFILES_POST:%=%.ml) \
	$(MLIFILES:%=%.mli)
SRCFILES= $(SRCMLFILES) $(PARSER:%=%.mly) $(LEXER=%=%.mll)
GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml)
CAMLFILES = $(SRCMLFILES) $(GENFILES)
CMXFILES = $(MLFILES_PRE:%=%.cmx) \
	$(LEXER:%=%.cmx) \
	$(PARSER:%=%.cmx) \
	$(MLFILES_TRANS:%=%.cmx) \
	$(MLFILES_POST:%=%.cmx)
CMIFILES= $(MLIFILES:%=%.cmi)
CMOFILES= $(CMXFILES:%.cmx=%.cmo)

LIBS = nums
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)


$(NAT): depend $(CMXFILES)
	@echo "EXE $@"
	@$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES)


$(BYT): depend $(CMIFILES) $(CMOFILES)
	@echo "EXE $@"
	@$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES)

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

license:
	@echo "Adding license prelude to files"
	@for i in $(SRCFILES); do \
		headache -r -h ../LICENSE $$i; \
	done
