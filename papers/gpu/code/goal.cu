// Here is what we want to generate. From:
// http://llpanorama.wordpress.com/2008/05/21/my-first-cuda-program/

// Build with something like:
// nvcc -I/usr/local/cuda/include/ -L/usr/local/cuda/lib/ goal.cu

#include <stdio.h>
#include <cuda.h>

// Kernel that executes on the CUDA device
__global__ void square_array(float *a, int N) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  if (idx<N) a[idx] = a[idx] * a[idx];
}

void handle_error(cudaError_t err) {
  printf("Error: %s\n",cudaGetErrorString(err));
  exit(1);
}

int main() {  
  // Pointer to host and device arrays
  float *a_h, *a_d;
  
  // Number of elements in arrays
  const int N = 10;
  size_t size = N * sizeof(float);
  
  cudaError_t err;
  
  int deviceCount = 0;
  err = cudaGetDeviceCount(&deviceCount);
  if(err) handle_error(err);
  printf("Found %d CUDA devices\n",deviceCount);
  
  // Allocate arrays
  a_h = (float *)malloc(size);
  err = cudaMalloc((void **) &a_d, size);
  if(err) handle_error(err);
  
  // Initialize host array
  for(int i=0; i<N; i++) {
    a_h[i] = (float)i;
  }
  
  // Copy host to device
  err = cudaMemcpy(a_d, a_h, size, cudaMemcpyHostToDevice);
  if(err) handle_error(err);
  
  // Do calculation on device
  int block_size = 4;
  int n_blocks = N/block_size + (N%block_size == 0 ? 0:1);
  square_array <<< n_blocks, block_size >>> (a_d, N);
  
  // Copy from device to host
  err = cudaMemcpy(a_h, a_d, size, cudaMemcpyDeviceToHost);
  if(err) handle_error(err);
  
  // Print results
  for (int i=0; i<N; i++) {
    printf("%d %f\n", i, a_h[i]);
  }
  
  // Cleanup
  free(a_h);
  cudaFree(a_d);
}
