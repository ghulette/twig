#include <stdio.h>
#include "routines.h"

int foo(int x, int y) {
  return x * y;
}

// How do you use this function from Python?  It returns the value in 
// the args.
void complex_mult(double r1, double i1, 
                  double r2, double i2, 
                  double *r3, double *i3)
{
  printf("Multiplying %0.2f+%0.2fi * %0.2f+%0.2fi\n", r1, i1, r2, i2);
  *r3 = r1 * r2 - i1 * i2;
  *i3 = r1 * i2 + r2 * i1;
}
