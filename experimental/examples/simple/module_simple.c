#include "Python.h"
#include "simple.h"

void handle_error(void) {
  PyErr_SetString(PyExc_RuntimeError, "Twig error");
}

static PyObject *simple_avg(PyObject *self, PyObject *args) {
  int arg1;
  int arg2;
  float result;
  PyObject *obj0;
  PyObject *obj1;
  PyObject *resultobj;
  
  if(!PyArg_ParseTuple(args,(char *)"OO:avg",&obj0,&obj1)) {
    handle_error();
    return NULL;
  }
  if(!PyInt_Check(obj0)) {
    handle_error();
    return NULL;
  }
  arg1 = (int)PyInt_AsLong(obj0);
  if(!PyInt_Check(obj1)) {
    handle_error();
    return NULL;
  }
  arg2 = (int)PyInt_AsLong(obj1);
  result = avg(arg1,arg2);
  resultobj = Py_BuildValue("f",result);
  return resultobj;
}

static PyObject *simple_output(PyObject *self, PyObject *args) {
  char *arg1;
  char *arg1_buf;
  Py_ssize_t arg1_len;
  PyObject *obj0;

  if(!PyArg_ParseTuple(args,(char *)"O:output",&obj0)) {
    handle_error();
    return NULL;
  }
  if(!PyString_Check(obj0)) {
    handle_error();
    return NULL;
  }
  arg1_buf = PyString_AsString(obj0);
  arg1_len = PyString_Size(obj0) + 1; // string length + null terminator
  arg1 = malloc(arg1_len * sizeof(char));
  if(arg1 == NULL) {
    handle_error();
    return NULL;
  }
  memcpy(arg1,arg1_buf,arg1_len);
  output(arg1);
  free(arg1);
  Py_RETURN_NONE;
}

static PyMethodDef SimpleMethods[] = {
    {"avg", simple_avg, METH_VARARGS, "Average."},
    {"output", simple_output, METH_VARARGS, "Print out."},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initsimple(void) {
    (void)Py_InitModule("simple", SimpleMethods);
}
