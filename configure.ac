AC_INIT()

has_or_die ()
{
    if test "$1" = no; then
        AC_MSG_ERROR($2)
    fi
}

AC_CHECK_PROG(MAKE, make, make.)

# ocamlc bytecode compiler
AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,no)
if test "$OCAMLC" = no; then
    AC_MSG_ERROR("Could not find ocamlc.")
else
    AC_CHECK_PROG(OCAMLCDOTOPT,ocamlc.opt,ocamlc.opt, no)
    if test "$OCAMLCDOTOPT" != no; then
        OCAMLC=$OCAMLCDOTOPT
    fi
    OCAMLBEST=byt
fi

AC_MSG_CHECKING([version of OCaml])
OCAMLCVERSION=`$OCAMLC -version`
LEAST_VERSION="4.02.1"
case $OCAMLCVERSION in
     4.04*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.03*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.02*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     *) AC_MSG_ERROR("Please use at least $LEAST_VERSION (found $OCAMLCVERSION).");;
esac


# ocamlopt native compiler
AC_CHECK_PROG(OCAMLOPT, ocamlopt, ocamlopt, no)

if test "$OCAMLOPT" = no; then
    AC_MSG_WARN("Bytecode compilation only.")
else
    AC_CHECK_PROG(OCAMLOPTDOTOPT,ocamlopt.opt,ocamlopt.opt,no)
    if test "$OCAMLOPTDOTOPT" != no; then
        OCAMLOPT=$OCAMLOPTDOTOPT
    fi
    OCAMLOPTVERSION=`$OCAMLOPT -version`
    OCAMLBEST=opt
    if test "$OCAMLOPTVERSION" != "$OCAMLCVERSION"; then
       AC_MSG_ERROR("OCaml's bytecode and optimized compilers have version number conflicts ($OCAMLCVERSION) vs ($OCAMLOPTVERSION) . Should be the same")
    fi
fi

# ocamldep
AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep, no)
has_or_die $OCAMLDEP "Could not find ocamldep"
OCAMLDEPVERSION=`ocamldep -version | sed -e 's/ocamldep, version \(.*\)/\1/'`
if test "$OCAMLDEPVERSION" != "$OCAMLCVERSION"; then
       AC_MSG_ERROR("ocamldep version $OCAMLCVERSION conflicts with ocamlc version $OCAMLCVERSION. Should be the same.")
fi

# ocamllex
AC_CHECK_PROG(OCAMLLEX,ocamllex,ocamllex,no)
has_or_die $OCAMLLEX "Could not find ocamllex"

# menhir
AC_CHECK_PROG(MENHIR,menhir,menhir,no)
has_or_die $MENHIR  "Could not find menhir"

# Optional libraries #
## ocamldoc
AC_CHECK_PROG(OCAMLDOC, ocamldoc, ocamldoc, no)
if test "$OCAMLDOC" = no; then
    AC_MSG_WARN("Documentation generation disabled.")
else
    OCAMLDOCVERSION=`ocamldoc -version`
fi

## headache
AC_CHECK_PROG(HEADACHE, headache, headache, no)
if test "$HEADACHE" = no; then
    AC_MSG_WARN("License generation disabled.")
fi

# Substitutions #

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(MENHIR)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLBEST)
AC_SUBST(HEADACHE)


AC_CONFIG_FILES([Config.mk], [chmod a-w Config.mk])
AC_OUTPUT

AS_ECHO(".depend: touching src/.depend")
touch src/.depend

AS_ECHO("")
AS_ECHO("")
AS_ECHO("Configuration summary")
AS_ECHO("---------------------")
AS_ECHO("ocamlc: $OCAMLC $OCAMLCVERSION")
AS_ECHO("ocamlopt: $OCAMLOPT $OCAMLOPTVERSION")
AS_ECHO("ocamldep: $OCAMLDEP $OCAMLDEPVERSION")
AS_ECHO("ocamllex: $OCAMLLEX")
AS_ECHO("menhir: $MENHIR")
AS_ECHO("ocamldoc: $OCAMLDOC $OCAMLDOCVERSION")
AS_ECHO("target: $OCAMLBEST")
AS_ECHO("headache: $HEADACHE")
