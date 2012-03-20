// This is what we want to generate.

#include <jansson.h>

PyObject *call_python(char *module, char *f, PyObject *in) {
  PyObject *module,*dict,*func,*out;
  
  module = PyImport_ImportModule(module);
  dict = PyModule_GetDict(module);
  func = PyDict_GetItemString(dict,f);
  out  = PyEval_CallObject(func,in);
  return out;
}

void gen(PyObject *in) {
  // [py(address) -> py(json(int,string))]
  PyObject *x1 = call_python("target","gen1_py",in);

  // This is the problem?
  // [py(json(int,string)) -> json(int,string)]
  json_error_t tmp1; // <-- need to use tmps for this or blocks
  char *x2 = PyString_AsString(x1);
  json_t *x3 = json_loads(x2, JSON_DECODE_ANY, &tmp1);
  
  // [json(int,string) -> (json(int),json(string))]
  json_t *x4 = json_array_get(x3,0);
  json_t *x5 = json_array_get(x3,1);
  
  // [json(int) -> int]
  int x6 = json_integer_value(x4);

  // [json(string) -> string]
  char *x7 = json_string_value(x5);
}
