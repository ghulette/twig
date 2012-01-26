#include <stdio.h>
#include <math.h>

struct PolarF {
  float r;
  float theta;
};

struct PolarD {
  double dist;
  double angle;
};

struct Pt {
  float x;
  float y;
};

struct Pt polarToPt(struct PolarF gen1) {
  float gen2,gen3,gen4,gen5;
  struct Pt gen6;
  gen2 = gen1.r;
  gen3 = gen1.theta;
  gen4 = gen2 * cosf(gen3);
  gen5 = gen2 * sinf(gen3);
  gen6.x = gen4;
  gen6.y = gen5;
  return gen6;
}

int main() {
  return 0;
}
