AC_INIT(obaf,0.1~dev)
#AC_CONFIG_MACRO_DIR([m4])
m4_include([m4/ocaml.m4])

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "$OCAMLOPT" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "$OCAMLDEP" = "no"; then
  AC_MSG_WARN([Cannot find ocamldep.])
fi

AC_PROG_CAMLP4
if test "$CAMLP4" = "no"; then
  AC_MSG_ERROR([Cannot find camlp4.])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_MSG_CHECKING(presence of Capstone OCaml binding)
CAPSTONE_PATH=`locate capstone.ml -l1`
if test "$CAPSTONE_PATH" = ""; then
  AC_MSG_ERROR([Cannot find Capstone.])
else
  CAPSTONE=`dirname $CAPSTONE_PATH`
fi

AC_MSG_RESULT($CAPSTONE)
AC_SUBST(CAPSTONE)
AC_OUTPUT(Makefile)
