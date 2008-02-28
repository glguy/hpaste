#include <python2.5/Python.h>

void py_decref(PyObject *o) { Py_DECREF(o); }
