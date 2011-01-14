// This is the given function.
void get_line(char *prompt, char *buffer) {
  printf("%s", prompt);
  scanf("%s", buffer);
}

// This is what I need to generate.
JNIEXPORT jstring JNICALL
Java_Prompt_getLine (JNIEnv *, jobject, jstring) {
  // We assume here that the user does not type more than 127 characters.
  char buf[128];
  const jbyte *str;
  str = (*env)->GetStringUTFChars(env, prompt, NULL);
  if (str == NULL) {
    // OutOfMemoryError already thrown.
    return NULL;
  }
  get_line(str,buf);
  (*env)->ReleaseStringUTFChars(env, prompt, str);
  return (*env)->NewStringUTF(env, buf);
}
