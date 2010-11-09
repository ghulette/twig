%module routines
%{
#include "routines.h"
%}

int foo(int x, int y);

void complex_mult(double r1, double i1, double r2, double i2, 
  double *r3, double *i3);
 