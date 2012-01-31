%module polar
%{
#include "polar.h"
%}

// "in" typemaps map a *single* Python object to the indicated C type
%typemap(in) (double,double) %{
  $1 = PyFloat_AsDouble($input);
  $2 = PyFloat_AsDouble($input);
%}

double polarToX(struct Polar);
double polarToY(struct Polar);
