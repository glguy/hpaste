#ifndef __PYTHON_LOCAL_H
#define __PYTHON_LOCAL_H
#include <python2.5/Python.h>

extern void py_decref(PyObject *o);
extern PyObject* py_return_true();
extern PyObject* py_return_false();
extern PyObject* py_unicode_decode_utf8(const char *a, int s);
extern PyObject *py_unicode_as_utf8(PyObject *a);

#endif
