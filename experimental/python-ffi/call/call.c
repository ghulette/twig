// Test program that calls a Python function

#include "Python.h"

int main() {
  PyObject *args,*a,*b,*c;
  PyObject *module,*dict,*func;
  
  Py_Initialize();
  PySys_SetPath(".");
  module = PyImport_ImportModule("Test");
  dict = PyModule_GetDict(module);
  func = PyDict_GetItemString(dict,"foo");
  a = PyInt_FromLong(3);
  b = PyInt_FromLong(5);
  args = PyTuple_Pack(2,a,b);
  if(!PyCallable_Check(func)) {
    printf("Not callable\n");
    return -1;
  }
  c = PyEval_CallObject(func,args);
  printf("result = %ld\n", PyInt_AsLong(c));
  return 0;
}
