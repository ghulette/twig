#ifndef GPU_H
#define GPU_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
  char id[64];
  int len;
  double *data;
} GPU;

GPU *init_gpu();
void cleanup_gpu(GPU *gpu);
void copy_array_to_gpu(GPU *gpu,double *src,int len);
void copy_array_from_gpu(GPU *gpu,double *dst,int len);
void run_kernel(GPU *gpu,char *kernel);

#endif
