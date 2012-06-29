struct PolarD {
  double r;
  double theta;
};

struct PolarF {
  float r;
  float theta;
};

double dist(struct PolarD p1, struct PolarD p2);
float distf(struct PolarF p1, struct PolarF p2);
struct PolarD origin();
struct PolarF originf();
