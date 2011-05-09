#include <stdio.h>
#include <stdlib.h>

// Sum and return the numbers in array x.  Also zero each element of x.
int foo(int *x, int n) {
  int s = 0,i;
  for(i = 0; i < n; i ++) {
    s += x[i];
    x[i] = 0;
  }
  return s;
}

// struct foreign_int {
//   int n;
// };

typedef int foreign_int;

foreign_int wrap_int(int x) {
  return x;
}

int unwrap_int(foreign_int x) {
  return x;
}

// Wrap foo.  Notice how each element must be converted individually.
foreign_int twig_foo(foreign_int *x, foreign_int n) {
  int twig_n = unwrap_int(n);
  int *twig_x = (int *)malloc(n * sizeof(int));
  int i;
  for(i = 0; i < n; i ++) {
    twig_x[i] = wrap_int(x[i]);
  }
  int twig_result = foo(twig_x,twig_n);
  for(i = 0; i < n; i ++) {
    x[i] = wrap_int(twig_x[i]);
  }
  free(twig_x);
  return wrap_int(twig_result);
}

int main() {
  int xs[5] = {1,2,3,4,5};
  printf("%d\n", twig_foo(xs,5));
  int i;
  for(i = 0; i < 5; i ++) {
    printf("%d\n", xs[i]);
  }
  return 0;
}
