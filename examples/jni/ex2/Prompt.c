#include <stdio.h>
#include <jni.h>

// This is the library function to wrap
void get_line(char *prompt, char *buffer) {
  printf("%s", prompt);
  scanf("%s", buffer);
}

// This is what Twig needs to generate, starting with this:
// void get_line(char *prompt, char *buffer)
JNIEXPORT jstring JNICALL
Java_Prompt_getLine (JNIEnv *env, jobject self, jstring prompt) {
  
  // #1 INIT - Convert prompt from Java to C string
  const jbyte *str;
  str = (*env)->GetStringUTFChars(env, prompt, NULL);
  if (str == NULL) {
    // OutOfMemoryError already thrown.
    return NULL;
  }
  char *cstr = (char *)str; // conversion "function"
  
  // #2 INIT - Invoke inner function, with an output parameter buf
  char buf[128];
  get_line(cstr,buf);
  
  // #3 INIT - Create a new string from the out parameter
  jstring result = (*env)->NewStringUTF(env, buf);
  
  
  // #1 FINAL - Release memory for C string
  (*env)->ReleaseStringUTFChars(env, prompt, str);
  
  
  return result;
}
