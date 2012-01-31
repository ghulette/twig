%module polar
%{
#include "polar.h"
%}

// This is an "in" typemap, which maps a Python object to a C type. We are 
// expecting the Python caller to pass us Cartesian coordinates. So, we have to 
// convert them.
%typemap(in) struct PolarD %{
  {
  // Unpack Python tuple
  PyObject *px = PyTuple_GetItem($input,0);
  PyObject *py = PyTuple_GetItem($input,1);
  
  // Convert x and y from Python to C
  double x = PyFloat_AsDouble(px);
  double y = PyFloat_AsDouble(py);
  
  // Perform translation from Cartesian to polar
  double r = sqrt(x * x + y * y);
  double theta = atan2(y,x);

  // Pack struct
  $1.r = r;
  $1.theta = theta;
  }
%}

%typemap(in) struct PolarF %{
  {
  // Unpack Python tuple
  PyObject *px = PyTuple_GetItem($input,0);
  PyObject *py = PyTuple_GetItem($input,1);
  
  // Convert x and y from Python to C
  float x = (float)PyFloat_AsDouble(px);
  float y = (float)PyFloat_AsDouble(py);
  
  // Perform translation from Cartesian to polar
  float r = sqrtf(x * x + y * y);
  float theta = atan2f(y,x);

  // Pack struct
  $1.r = r;
  $1.theta = theta;
  }
%}

%typemap(out) struct PolarD %{
  {
  // Unpack struct
  double r = $1.r;
  double theta = $1.theta;
  
  // Perform translation from polar to Cartesian
  double x = r * cos(theta);
  double y = r * sin(theta);
  
  // Convert x and y from Python to C
  PyObject *px = PyFloat_FromDouble(x);
  PyObject *py = PyFloat_FromDouble(y);
  
  // Pack Python tuple
  $result = PyTuple_Pack(2,px,py);
  }
%}

%typemap(out) struct PolarF %{
  {
  // Unpack struct
  float fr = $1.r;
  float ftheta = $1.theta;
  
  // convert to double
  double r = (double)fr;
  double theta = (double)ftheta;
  
  // Perform translation from polar to Cartesian
  double x = r * cos(theta);
  double y = r * sin(theta);
  
  // Convert x and y from Python to C
  PyObject *px = PyFloat_FromDouble(x);
  PyObject *py = PyFloat_FromDouble(y);
  
  // Pack Python tuple
  $result = PyTuple_Pack(2,px,py);
  }
%}

double dist(struct PolarD p1, struct PolarD p2);
float distf(struct PolarF p1, struct PolarF p2);
struct PolarD origin();
struct PolarF originf();
