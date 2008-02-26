{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C

data PyObjectStruct = PyObjectStruct
type PyObject = Ptr PyObjectStruct
type PyObj = ForeignPtr PyObjectStruct

foreign import ccall "python2.5/Python.h Py_Initialize"
  pyInitialize :: IO ()

foreign import ccall "python2.5/Python.h Py_Finalize"
  pyFinalize :: IO ()

foreign import ccall "python2.5/Python.h PyRun_SimpleString"
  pyRunSimpleString :: CString -> IO ()

foreign import ccall "python2.5/Python.h PyImport_ImportModule"
  pyImportImportModule :: CString -> IO PyObject

foreign import ccall "python2.5/Python.h PyObject_GetAttrString"
  pyObjectGetAttrString :: PyObject -> CString -> IO PyObject

foreign import ccall "python-local.h py_decref"
  pyDecRef :: PyObject -> IO ()

foreign import ccall "python-local.h &py_decref"
  ppyDecRef :: FinalizerPtr PyObjectStruct

foreign import ccall "python2.5/Python.h PyEval_CallObject"
  pyEvalCallObject :: PyObject -> PyObject -> IO PyObject

foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue0 :: CString -> IO PyObject

foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue1 :: CString -> CString -> IO PyObject

foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue3 :: CString -> PyObject -> PyObject -> PyObject -> IO PyObject

foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction0 :: PyObject -> CString -> IO PyObject

foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction1 :: PyObject -> CString -> Ptr a -> IO PyObject

foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction3 :: PyObject -> CString -> Ptr a -> Ptr b -> Ptr c -> IO PyObject

foreign import ccall "python2.5/Python.h PyArg_Parse"
  pyParse :: PyObject -> CString -> Ptr CString -> IO CInt

importModule :: String -> IO PyObject
importModule module_name = withCString module_name pyImportImportModule

fetch :: PyObject -> String -> IO PyObject
fetch pmod object_name = withCString object_name $ pyObjectGetAttrString pmod

buildValue1 pat val = withCString pat $ \ ppat -> withCString val $ \ pval ->
                        pyBuildValue1 ppat pval

buildValue3 pat a b c = withCString pat $ \ ppat -> pyBuildValue3 ppat a b c

fromImport mod klass =
 do pmod <- importModule mod
    obj <- fetch pmod klass
    pyDecRef pmod
    newForeignPtr ppyDecRef obj

toString obj = 
  alloca $ \ cstr_ptr ->
  withCString "s" $ \ s ->
  withForeignPtr obj $ \ r ->
  pyParse r s cstr_ptr >>
  peek cstr_ptr >>= peekCString

-- from pygments import highlight
-- from pygments.lexers import PythonLexer
-- from pygments.formatters import HtmlFormatter
-- code = 'asdf'
-- lexer = get_lexer_by_name("python", stripall=True)
-- formatter = HtmlFormatter(linenos=True, cssclass="source")
-- result = highlight(code, lexer, formatter)
mycode =
 do let code = "instance Functor f where\n fmap :: f a"

    get_lexer <- fromImport "pygments.lexers" "get_lexer_by_name"
    lexer     <- call1 get_lexer "haskell"
    formatter <- call0 =<< fromImport "pygments.formatters" "HtmlFormatter"
    highlight <- fromImport "pygments" "highlight"
    res       <- call3 highlight code lexer formatter
    putStrLn =<< toString res

main = pyInitialize >> mycode >> pyFinalize

call0 :: PyObj -> IO PyObj
call0 f = withForeignPtr f $ \ fp ->
          withCString "()" $ \ format ->
          pyCallFunction0 fp format >>= \ res ->
          newForeignPtr ppyDecRef res

call1 :: PyArg a _b => PyObj -> a -> IO PyObj
call1 f a = withForeignPtr f $ \ fp ->
            withCString ('(':format_char a:")") $ \ format ->
            apply_arg a $ \ aa ->
            pyCallFunction1 fp format aa >>= \ res ->
            newForeignPtr ppyDecRef res

call3 :: (PyArg a _b, PyArg c _d, PyArg e _f)
      => PyObj -> a -> c -> e -> IO PyObj
call3 f a b c = withForeignPtr f $ \ fp ->
                withCString format_string $ \ format ->
                apply_arg a $ \ aa ->
                apply_arg b $ \ bb ->
                apply_arg c $ \ cc ->
                pyCallFunction3 fp format aa bb cc >>= \ res ->
                newForeignPtr ppyDecRef res
  where format_string = '(':format_char a:format_char b:format_char c:")"

class PyArg a b | a -> b where
  format_char :: a -> Char
  apply_arg :: a -> (Ptr b -> IO PyObj) -> IO PyObj

instance PyArg String CChar where
  format_char _ = 's'
  apply_arg = withCString

instance PyArg PyObj PyObjectStruct where
  format_char _ = 'O'
  apply_arg = withForeignPtr

instance PyArg CInt () where
  format_char _ = 'i'
  apply_arg a f = f (wordPtrToPtr (fromIntegral a))
