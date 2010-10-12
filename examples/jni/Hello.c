#include <jni.h> 
#include <stdio.h> 
#include "Hello.h"

JNIEXPORT void JNICALL 
Java_Hello_print(JNIEnv *env, jobject obj) {
  printf("Hello World!\n"); 
  return;
}
