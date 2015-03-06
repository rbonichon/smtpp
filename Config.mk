CAMLBYT=ocamlc.opt
CAMLBIN=ocamlopt.opt
CAMLLEX=ocamllex
CAMLLEXOPTS=
CAMLYAC=menhir
CAMLYACOPTS=
RM=rm -f
MKDIR=mkdir -p
CAMLDEP=ocamldep
CAMLFLAGS=-w +a-4 -g -annot
CAMLDOC=ocamldoc
HEADACHE=headache
BEST=opt

default: $(BEST)

.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES: .o .c
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly


.cmx.o:
	$(CAMLBIN) -output-obj -o $@ $<

.c.o:
	$(CAMLBIN) -ccopt "-fPIC -o $@" -c $<

.ml.cmo:
	@echo "BYT $<"
	@$(CAMLBYT)  $(CAMLINCLUDES) $(CAMLFLAGS) -c $<

.mli.cmi:
	@echo "INT $<"
	@$(CAMLBYT)  $(CAMLINCLUDES) -c $<

.ml.cmx:
	@echo "OPT $<"
	@$(CAMLBIN) $(CAMLINCLUDES) $(CAMLFLAGS) -c  $<

.mly.ml:
	@echo "YACC $<"
	@$(CAMLYAC) $(CAMLYACOPTS) $<

.mly.mli:
	@echo "YACC $<"
	$(CAMLYAC)  $(CAMLYACOPTS) $<

.mll.ml:
	@echo "LEX $<"
	@$(CAMLLEX) $(CAMLLEXOPTS) $<

# Generic clean up
cleandir::
	$(RM) *.cm[ioxa] *.cmxa *.o *.a *.annot *.obj *.lib *~ .*~ a.out .\#*

clean:: cleandir
	($(MAKE) depend) || exit $$?

configure:: cleandir

# Rebuilding dependencies
depend::
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

include .depend
