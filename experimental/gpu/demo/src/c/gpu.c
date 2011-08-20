#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gpu.h"

GPU *init_gpu() {
  GPU *gpu = malloc(sizeof(GPU));
  strcpy(gpu->id,"Psuedo GPU");
  gpu->data = NULL;
  gpu->len = 0;
  return gpu;
}

void cleanup_gpu(GPU *gpu) {
  if(gpu->data != NULL) {
    free(gpu->data);
  }
  free(gpu);
}

void copy_array_to_gpu(GPU *gpu,double *src,int len) {
  if(gpu->data != NULL) {
    free(gpu->data);
  }
  gpu->data = malloc(len * sizeof(double));
  gpu->len = len;
  int i;
  for(i=0; i < len; i++) {
    gpu->data[i] = src[i];
  }
}

void copy_array_from_gpu(GPU *gpu,double *dst,int len) {
  int i;
  for(i=0; i < gpu->len; i++) {
    dst[i] = gpu->data[i];
  }
}

void run_kernel(GPU *gpu,char *kernel) {
  int i;
  for(i=0; i < gpu->len; i++) {
    // useless kernel operation
    gpu->data[i] /= 2.0;
  }
}
