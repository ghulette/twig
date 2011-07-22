#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <jni.h>
#include "gpu.h"
#include "Convolution.h"

void copy_java_array_to_gpu(GPU *gpu,JNIEnv *env,jdoubleArray jsrc) {
  if(gpu->data != NULL) {
    free(gpu->data);
  }
  int len = (*env)->GetArrayLength(env,jsrc);
  gpu->data = malloc(len * sizeof(double));
  gpu->len = len;
  (*env)->GetDoubleArrayRegion(env,jsrc,0,len,gpu->data);
}

jdoubleArray copy_java_array_from_gpu(GPU *gpu,JNIEnv *env) {
  jdoubleArray jdst = (*env)->NewDoubleArray(env,gpu->len);
  (*env)->SetDoubleArrayRegion(env,jdst,0,gpu->len,gpu->data);
  return jdst;
}

JNIEXPORT jdoubleArray JNICALL Java_Convolution_applyOpt
  (JNIEnv *env,jobject obj,jdoubleArray jsrc)
{
  // Get array length
  int len = (*env)->GetArrayLength(env,jsrc);
  
  // Initialize GPU
  GPU *gpu = init_gpu();
  
  // Copy array from Java to GPU
  copy_java_array_to_gpu(gpu,env,jsrc);
  
  // Invoke kernel
  run_kernel(gpu,"convolve");
  
  // Copy array from GPU to Java
  jdoubleArray jdst = copy_java_array_from_gpu(gpu,env);
  
  // Cleanup GPU
  cleanup_gpu(gpu);
  
  // Clean up and return
  return jdst;
}

JNIEXPORT jdoubleArray JNICALL Java_Convolution_apply
  (JNIEnv *env,jobject obj,jdoubleArray jsrc)
{
  // Get array length
  int len = (*env)->GetArrayLength(env,jsrc);
  
  // Copy array from Java to C
  double *src = malloc(len * sizeof(double));
  (*env)->GetDoubleArrayRegion(env,jsrc,0,len,src);
  
  // Initialize GPU
  GPU *gpu = init_gpu();
  
  // Copy array from C to GPU
  copy_array_to_gpu(gpu,src,len);
  
  // Invoke kernel
  run_kernel(gpu,"convolve");
  
  // Copy array from GPU to C
  double *dst = malloc(len * sizeof(double));
  copy_array_from_gpu(gpu,dst,len);
  
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