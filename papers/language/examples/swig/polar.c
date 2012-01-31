#include "math.h"
#include "polar.h"

double polarToX(struct Polar p) {
  return p.r * cos(p.theta);
}

double polarToY(struct Polar p) {
  return p.r * sinf(p.theta);
}
