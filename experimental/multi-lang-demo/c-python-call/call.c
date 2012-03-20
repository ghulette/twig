// Test program that calls a Python function

#include "Python.h"

void do_call() {
  PyObject *args,*a,*b,*c;
  PyObject *module,*dict,*func;
  
  module = PyImport_ImportModule("Test");
  dict = PyModule_GetDict(module);
  func = PyDict_GetItemString(dict,"foo");
  a = PyInt_FromLong(3);
  b = PyInt_FromLong(5);
  args = PyTuple_Pack(2,a,b);
  c = PyEval_CallObject(func,args);
  printf("result = %ld\n", PyInt_AsLong(c));
}

int main() {
  Py_Initialize();
  PySys_SetPath(".");
  do_call();
  Py_Finalize();
  return 0;
}
