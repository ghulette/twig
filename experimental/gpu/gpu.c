#include <stdio.h>
#include <stdlib.h>

// Simplified GPU interface
extern void copy_to_gpu(int *x,int n);
extern void copy_from_gpu(int *x,int n);
extern void exec_gpu_kernel(char *id);

void foo(int *x, int n) {
  copy_to_gpu(x,n);
  exec_gpu_kernel("foo");
  copy_from_gpu(x,n);
}

void bar(int *x, int n) {
  copy_to_gpu(x,n);
  exec_gpu_kernel("bar");
  copy_from_gpu(x,n);
}

int main() {
  int n = 1000;
  int *x = (int *)calloc(sizeof(int),n);
  foo(x,n);
  bar(x,n);
  printf("Done\n");
  return 0;
}
