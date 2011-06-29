#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <jni.h>
#include "Convolution.h"

typedef struct {
  char id[64];
  int len;
  double *data;
}GPU;

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

void copy_to_gpu(GPU *gpu,double *src,int len) {
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

void copy_from_gpu(GPU *gpu,double *dst,int len) {
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

JNIEXPORT jdoubleArray JNICALL Java_Convolution_apply
  (JNIEnv *env, jobject obj, jdoubleArray jsrc)
{
  // Get array length
  int len = (*env)->GetArrayLength(env,jsrc);
  
  // Copy array from Java to C
  double *src = malloc(len * sizeof(double));
  (*env)->GetDoubleArrayRegion(env,jsrc,0,len,src);
  
  // Initialize GPU
  GPU *gpu = init_gpu();
  
  // Copy array from C to GPU
  copy_to_gpu(gpu,src,len);
  
  // Invoke kernel
  run_kernel(gpu,"convolve");
  
  // Copy array from GPU to C
  double *dst = malloc(len * sizeof(double));
  copy_from_gpu(gpu,dst,len);
  
  // Cleanup GPU
  cleanup_gpu(gpu);
  
  // Copy array from C to Java
  jdoubleArray jdst = (*env)->NewDoubleArray(env,len);
  (*env)->SetDoubleArrayRegion(env,jdst,0,len,dst);
  
  // Clean up and return
  free(dst);
  free(src);
  return jdst;
}
