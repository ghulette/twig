// Unoptimized version

#include <stdio.h>
#include <cuda.h>

// Kernel that executes on the CUDA device
__global__ void times_two(int *a, int N) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  if (idx<N) a[idx] *= 2;
}

// Kernel that executes on the CUDA device
__global__ void plus_one(int *a, int N) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  if (idx<N) a[idx] += 1;
}

void handle_error(cudaError_t err) {
  printf("Error: %s\n",cudaGetErrorString(err));
  exit(1);
}

int main() {
  // Result codes
  cudaError_t err;
  
  // Pointer to host and device arrays
  int *a_h, *a_d;
  
  // Number of elements in arrays
  const int N = 128;
  size_t size = N * sizeof(int);
  int block_size = 4;
  int n_blocks = N/block_size + (N%block_size == 0 ? 0:1);
  
  // Query devices
  int deviceCount = 0;
  err = cudaGetDeviceCount(&deviceCount);
  if(err) handle_error(err);
  printf("Found %d CUDA devices\n",deviceCount);
  
  cudaDeviceProp props;
  for(int i=0; i<deviceCount; i++) {
    err = cudaGetDeviceProperties(&props,0);
    if(err) handle_error(err);
    printf("Using: %s\n",props.name);
  }
  
  // Allocate and initialize host array
  a_h = (int *)malloc(size);
  for(int i=0; i<N; i++) {
    a_h[i] = i;
  }
  
  // Allocate CUDA array
  err = cudaMalloc((void **) &a_d, size);
  if(err) handle_error(err);
  
// ===========================
// =        Block #1         =
// ===========================
  // Copy host to device
  err = cudaMemcpy(a_d, a_h, size, cudaMemcpyHostToDevice);
  if(err) handle_error(err);
  // Do calculation on device
  times_two <<< n_blocks, block_size >>> (a_d, N);
  // Wait for device to finish
  err = cudaThreadSynchronize();
  if(err) handle_error(err);
  // Copy from device to host
  err = cudaMemcpy(a_h, a_d, size, cudaMemcpyDeviceToHost);
  if(err) handle_error(err);

// ===========================
// =        Block #2         =
// ===========================
  // Copy host to device
  err = cudaMemcpy(a_d, a_h, size, cudaMemcpyHostToDevice);
  if(err) handle_error(err);
  // Do calculation on device
  plus_one <<< n_blocks, block_size >>> (a_d, N);
  // Wait for device to finish
  err = cudaThreadSynchronize();
  if(err) handle_error(err);
  // Copy from device to host
  err = cudaMemcpy(a_h, a_d, size, cudaMemcpyDeviceToHost);
  if(err) handle_error(err);

  // Print results
  for(int i=0; i<N; i++) {
    printf("%d %d\n", i, a_h[i]);
  }
  
  // Cleanup
  free(a_h);
  cudaFree(a_d);
}
