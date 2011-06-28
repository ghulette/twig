#include <stdio.h>
#include <jni.h>

JNIEXPORT void JNICALL Java_Convolution_test(JNIEnv *env, jobject obj) {
  printf("Hello JNI\n");
}
