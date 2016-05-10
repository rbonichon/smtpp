.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES: .o .c
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly


.cmo.o:
	$(PP) "COBJ $@"
	$(CAMLBYT) -custom -output-obj -o $@ $<

.c.o:
	$(CAMLBYT) -ccopt "-fPIC -o $@" -c $<

.ml.cmo:
	$(PP_BYT) $@
	$(CAMLBYT)  $(CAMLINCLUDES) $(CAMLFLAGS) -c $<

.mli.cmi:
	$(PP_BYT) $@
	$(CAMLBYT)  $(CAMLINCLUDES) -c $<

.ml.cmx:
	$(PP_OPT) $@
	$(CAMLBIN) $(CAMLINCLUDES) $(CAMLFLAGS) -c  $<

.mly.ml:
	$(PP_YACC) $@
	$(CAMLYAC) --trace $(CAMLYACOPTS) $<

.mly.mli:
	$(PP_YACC) $@
	$(CAMLYAC)  $(CAMLYACOPTS) $<

.mll.ml:
	$(PP_LEX) $@
	$(CAMLLEX) $(CAMLLEXOPTS) $<

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
