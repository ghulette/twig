#!/usr/bin/env bash

swig -python simple.i
gcc -c -I/usr/include/python2.6 simple.c simple_wrap.c
gcc -bundle -flat_namespace -undefined suppress -o _simple.so \
  simple.o simple_wrap.o
python test.py
