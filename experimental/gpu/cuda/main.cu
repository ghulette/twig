#include <stdio.h>
#include <cuda.h>

const int N = 10;
const size_t SIZE = N * sizeof(float);
const int BLOCK_SIZE = 4;
const int N_BLOCKS = N / BLOCK_SIZE + (N % BLOCK_SIZE == 0 ? 0:1);

__global__ void foo(float *a, int N) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  if(idx<N) {
    a[idx] = a[idx] * a[idx];
  }
}

__global__ void bar(float *a, int N) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  if(idx<N) {
    a[idx] = a[idx] + 1;
  }
}

#include "twig.cu"

int main(void) {
  float *input, *result;
  input = (float *)malloc(SIZE);
  for(int i=0; i < N; i++) {
    input[i] = (float)i;
  }
  result = twig_gen_fun(input);
  for(int i=0; i < N; i++) {
    printf("%d -> %f\n", i, result[i]);
  }
  return 0;
}
