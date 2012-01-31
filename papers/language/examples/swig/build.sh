#!/usr/bin/env bash

swig -python polar.i
gcc -c -I/usr/include/python2.6 -L/usr/lib/python2.6 polar.c polar_wrap.c
gcc -bundle -flat_namespace -undefined suppress \
  polar.o polar_wrap.o -o _polar.so
