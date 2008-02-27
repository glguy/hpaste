#include <python2.5/Python.h>

void py_decref(PyObject *o) { Py_DECREF(o); }
void py_incref(PyObject *o) { Py_INCREF(o); }
PyObject *py_return_true() { Py_RETURN_TRUE; }
PyObject *py_return_false() { Py_RETURN_FALSE; }
PyObject *py_unicode_decode_utf8(const char *a, int b) { return PyUnicode_DecodeUTF8(a,b,"strict"); }
PyObject *py_unicode_as_utf8(PyObject *a) { return PyUnicode_AsUTF8String(a); }
int py_iter_check(PyObject *a) { return PyIter_Check(a); }
PyObject *py_none = Py_None;
