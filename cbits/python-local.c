#include <python2.5/Python.h>

void py_decref(PyObject *o) { Py_DECREF(o); }
PyObject *py_return_true() { Py_RETURN_TRUE; }
PyObject *py_return_false() { Py_RETURN_FALSE; }
PyObject *py_unicode_decode_utf8(const char *a, int b) { return PyUnicode_DecodeUTF8(a,b,"strict"); }
PyObject *py_unicode_as_utf8(PyObject *a) { return PyUnicode_AsUTF8String(a); }
