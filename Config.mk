CAMLBYT=ocamlc.opt
CAMLBIN=ocamlopt.opt
CAMLLEX=ocamllex
CAMLLEXOPTS?=
CAMLYAC=menhir
CAMLYACOPTS?=
RM=rm -f
MKDIR=mkdir -p
CAMLDEP=ocamldep
CAMLFLAGS=-w +a-4 -g -annot
CAMLDOC=ocamldoc
HEADACHE=headache
BEST=opt
VERBOSEMAKE?=no

ifneq ($(VERBOSEMAKE),no) # Do not change to ifeq ($(VERBOSEMAKE),yes), as this
			  # version makes it easier for the user to set the
			  # option on the command-line to investigate
			  # Makefile-related problems
# ignore the PRINT_* materials but print all the other commands
  PP = @true
# prevent the warning "jobserver unavailable: using -j1".
# see GNU make manual (section 5.7.1 and appendix B)
  QUIET_MAKE:= + $(MAKE)
# prevent the warning: "-jN forced in submake: disabling jobserver mode".
# see GNU make manual (appendix B)
  MAKE := MAKEFLAGS="$(patsubst j,,$(MAKEFLAGS))" $(MAKE)
else
# print the PP_* materials
  PP = @echo
# but silently execute all the other commands
# fixed bug #637: do not write spaces between flags
  OLDFLAGS:=r$(MAKEFLAGS)
  MAKEFLAGS:=rs$(MAKEFLAGS)
# do not silently execute other makefiles (e.g the one of why):
# the redefinition of MAKE below is for this purpose
# but use QUIET_MAKE in order to call silently the initial Makefile
  QUIET_MAKE:= $(MAKE)
  MAKE := MAKEFLAGS="$(OLDFLAGS)" $(MAKE)
endif

PP_BYT   = $(PP) 'BYT  '
PP_OPT   = $(PP) 'BIN  '
PP_YACC  = $(PP) 'YACC '
PP_LEX   = $(PP) 'LEX  '
