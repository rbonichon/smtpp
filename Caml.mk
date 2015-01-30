CAMLBYT=ocamlc
CAMLBIN=ocamlopt
CAMLLEX=ocamllex
CAMLLEXOPTS=
CAMLYAC=menhir
CAMLYACOPTS=
RM=rm -f
CAMLDEP=ocamldep
CAMLFLAGS=-w +a-4 -g -annot

.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES:
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly


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
