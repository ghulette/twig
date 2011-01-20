#include <stdio.h>
#include <jni.h>

// This is the library function to wrap
void get_line(char *prompt, char *buffer) {
  printf("%s", prompt);
  scanf("%s", buffer);
}

// This is what Twig needs to generate
JNIEXPORT jstring JNICALL
Java_Prompt_getLine (JNIEnv *env, jobject self, jstring prompt) {
  // We assume here that the user does not type more than 127 characters.
  char buf[128];
  const jbyte *str;
  str = (*env)->GetStringUTFChars(env, prompt, NULL);
  if (str == NULL) {
    // OutOfMemoryError already thrown.
    return NULL;
  }
  char *cstr = (char *)str; // conversion "function"
  get_line(cstr,buf);
  jstring result = (*env)->NewStringUTF(env, buf);
  (*env)->ReleaseStringUTFChars(env, prompt, str);
  return result;
}
