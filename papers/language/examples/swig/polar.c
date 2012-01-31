#include "math.h"
#include "polar.h"

double dist(struct PolarD p1, struct PolarD p2) {
  double f1 = p1.r * p1.r;
  double f2 = p2.r * p2.r;
  double f3 = 2.0 * p1.r * p2.r * cos(p1.theta - p2.theta);
  return sqrt(f1 + f2 - f3);
}

float distf(struct PolarF p1, struct PolarF p2) {
  float f1 = p1.r * p1.r;
  float f2 = p2.r * p2.r;
  float f3 = 2.0 * p1.r * p2.r * cosf(p1.theta - p2.theta);
  return sqrtf(f1 + f2 - f3);
}

