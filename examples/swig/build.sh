#!/usr/bin/env bash

swig -python routines.i
gcc -c -I/usr/include/python2.6 -L/usr/lib/python2.6 \
  routines.c routines_wrap.c
gcc -bundle -flat_namespace -undefined suppress \
  routines.o routines_wrap.o -o _routines.so
