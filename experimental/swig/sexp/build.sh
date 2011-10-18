#!/usr/bin/env bash
swig -python sexp.i
gcc -c -I$SEXP_HOME/src -I/usr/include/python2.6 -L/usr/lib/python2.6 \
  sexp_wrap.c
gcc -bundle -flat_namespace -undefined suppress \
  -L$SEXP_HOME/src -lsexp sexp_wrap.o -o _sexp.so
