#include <stdio.h>
#include <stdlib.h>

float avg(int x, int y) {
  float sum = (float)(x + y);
  float avg = sum / 2.0f;
  return avg;
}

void output(char *msg) {
  printf("%s is the message\n", msg);
}
