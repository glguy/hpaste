{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}
module Python (main, withPython, highlightAs, get_languages) where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C
import Control.Monad
import Control.Exception (bracket_)

import Codec.Binary.UTF8.String as UTF8
import qualified System.IO.UTF8

data PyObjectStruct = PyObjectStruct
type PyObject = Ptr PyObjectStruct
type PyObj = ForeignPtr PyObjectStruct

foreign import ccall "python2.5/Python.h Py_Initialize"
  pyInitialize :: IO ()

foreign import ccall "python2.5/Python.h Py_Finalize"
  pyFinalize :: IO ()

foreign import ccall "python2.5/Python.h PyRun_SimpleString"
  pyRunSimpleString :: CString -> IO ()

-- New Reference
foreign import ccall "python2.5/Python.h PyImport_ImportModule"
  pyImportImportModule :: CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyObject_GetAttrString"
  pyObjectGetAttrString :: PyObject -> CString -> IO PyObject

foreign import ccall "python-local.h py_incref"
  pyIncRef :: PyObject -> IO ()

foreign import ccall "python-local.h py_decref"
  pyDecRef :: PyObject -> IO ()

foreign import ccall "python-local.h &py_decref"
  ppyDecRef :: FinalizerPtr PyObjectStruct

wrapPtr p = newForeignPtr ppyDecRef p

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallObject"
  pyEvalCallObject :: PyObject -> PyObject -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue0 :: CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue1 :: CString -> Ptr a -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue3 :: CString -> PyObject -> PyObject -> PyObject -> IO PyObject

foreign import ccall "python2.5/Python.h PyArg_Parse"
  pyParse :: PyObject -> CString -> Ptr CString -> IO CInt

-------------------------------------------------------------------------------
-- Dictionary Objects
-------------------------------------------------------------------------------
foreign import ccall "python2.5/Python.h PyDict_New"
  pyDictNew :: IO PyObject

foreign import ccall "python2.5/Python.h PyDict_SetItem"
  pyDictSetItem :: PyObject -> PyObject -> PyObject -> IO CInt

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyDict_GetItem"
  pyDictGetItem :: PyObject -> PyObject -> IO PyObject

foreign import ccall "python2.5/Python.h PyDict_SetItemString"
  pyDictSetItemString :: PyObject -> CString -> PyObject -> IO CInt

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyDict_GetItemString"
  pyDictGetItemString :: PyObject -> CString -> IO PyObject

foreign import ccall "python2.5/Python.h PyDict_DelItem"
  pyDictDelItem :: PyObject -> PyObject -> IO CInt

foreign import ccall "python2.5/Python.h PyDict_Clear"
  pyDictClear :: PyObject -> IO ()

dictNew :: IO PyObj
dictNew = wrapPtr =<< pyDictNew

dictGetItem d k =
  withForeignPtr d $ \ dd ->
  withForeignPtr k $ \ kk ->
  pyDictGetItem dd kk >>= \ x ->
  pyIncRef x >>
  wrapPtr x

dictSetItem d k v =
  withForeignPtr d $ \ dd ->
  withForeignPtr k $ \ kk ->
  withForeignPtr v $ \ vv ->
  pyDictSetItem dd kk vv

dictDelItem d k =
  withForeignPtr d $ \ dd ->
  withForeignPtr k $ \ kk ->
  pyDictDelItem dd kk

dictSetItemString :: PyObj -> String -> PyObj -> IO CInt
dictSetItemString d k v =
  withForeignPtr d $ \ dd ->
  withCString    k $ \ kk ->
  withForeignPtr v $ \ vv ->
  pyDictSetItemString dd kk vv

dictGetItemString d k =
  withForeignPtr d $ \ dd ->
  withCString    k $ \ kk ->
  pyDictGetItemString dd kk >>= \ x ->
  pyIncRef x >>
  wrapPtr x

dictClear d = withForeignPtr d pyDictClear

dictFromList xs =
 do d <- pyDictNew
    mapM_ (aux d) xs
    wrapPtr d
 where
 aux d (k, v) =
   withCString k $ \ kk ->
   withForeignPtr v $ \ vv ->
   pyDictSetItemString d kk vv



importModule :: String -> IO PyObject
importModule module_name = withCString module_name pyImportImportModule

fetch :: PyObject -> String -> IO PyObject
fetch pmod object_name = withCString object_name $ pyObjectGetAttrString pmod

buildValue1 pat val =
  withCString pat $ \ ppat ->
  apply_arg val $ \ pval ->
  wrapPtr =<< pyBuildValue1 ppat pval

buildValue3 pat a b c =
  withCString pat $ \ ppat ->
  apply_arg a     $ \ aa   ->
  apply_arg b     $ \ bb   ->
  apply_arg c     $ \ cc   ->
  wrapPtr =<< pyBuildValue3 ppat aa bb cc

fromImport mod klass =
 do pmod <- importModule mod
    obj <- fetch pmod klass
    pyDecRef pmod
    wrapPtr obj

-- from pygments import highlight
-- from pygments.lexers import PythonLexer
-- from pygments.formatters import HtmlFormatter
-- code = 'asdf'
-- lexer = get_lexer_by_name("python", stripall=True)
-- formatter = HtmlFormatter(linenos=True, cssclass="source")
-- result = highlight(code, lexer, formatter)
highlightAs :: String -> String -> IO String
highlightAs _ "" = return ""
highlightAs lang code =
 do t         <- returnTrue

    get_lexer <- fromImport "pygments.lexers" "get_lexer_by_name"
    lexer     <- call1withKeywords get_lexer lang [("stripall", t)]

    no_lexer  <- isNull lexer
    if no_lexer then pyErrClear >> highlightAs "text" code else do
      formatter_class <- fromImport "pygments.formatters" "HtmlFormatter"
      formatter <- call0withKeywords formatter_class [("linenos", t)]

      highlight <- fromImport "pygments" "highlight"
      unicodeToString =<< call3 highlight code lexer formatter

get_languages :: IO [String]
get_languages =
 do lexers <- call0 =<< fromImport "pygments.lexers" "get_all_lexers"
    forEach lexers $ \ x -> tupleGetItem x 0 >>= toString

withPython m = bracket_ pyInitialize pyFinalize m

main = withPython $ do highlightAs "haskell" "sdfasdf"
                       highlightAs "skell" "asdf"
-------------------------------------------------------------------------------
-- Function Calling
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction0 :: PyObject -> CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction1 :: PyObject -> CString -> Ptr a -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction2 :: PyObject -> CString -> Ptr a -> Ptr b -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction3 :: PyObject -> CString -> Ptr a -> Ptr b -> Ptr c -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallObject"
  pyCallObject :: PyObject -> PyObject -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallObjectWithKeywords"
  pyCallObjectWithKeywords :: PyObject -> PyObject -> PyObject -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallMethod"
  pyCallMethod0 :: PyObject -> CString -> CString -> IO PyObject

callObjectWithKeywords f args kargs =
  withForeignPtr f $ \ ff ->
  withForeignPtr args $ \ aa ->
  withForeignPtr kargs $ \ kk ->
  wrapPtr =<< pyCallObjectWithKeywords ff aa kk

call0 :: PyObj -> IO PyObj
call0 f = withForeignPtr f $ \ fp ->
          withCString "()" $ \ format ->
          pyCallFunction0 fp format >>= wrapPtr

call0withKeywords :: PyObj -> [(String,PyObj)] -> IO PyObj
call0withKeywords f ks =
  withForeignPtr f $ \ fp ->
  dictFromList ks >>= \ k ->
  withForeignPtr k $ \ kk ->
  wrapPtr =<< pyCallObjectWithKeywords fp nullPtr kk

callMethod0 :: PyObj -> String -> IO PyObj
callMethod0 obj meth =
  withForeignPtr obj $ \ o ->
  withCString meth   $ \ m ->
  withCString "()"   $ \ format ->
  wrapPtr =<< pyCallMethod0 o m format

call1 :: PyArg a => PyObj -> a -> IO PyObj
call1 f a = withForeignPtr f $ \ fp ->
            withCString ('(':format_char a:")") $ \ format ->
            apply_arg a $ \ aa ->
            pyCallFunction1 fp format aa >>= wrapPtr

call1withKeywords :: PyArg a => PyObj -> a -> [(String,PyObj)] -> IO PyObj
call1withKeywords f a ks =
  withForeignPtr f $ \ ff ->
  dictFromList ks >>= \ k ->
  withForeignPtr k $ \ kk ->
  buildValue1 ('(':format_char a:")") a >>= \ args ->
  withForeignPtr args $ \ aa ->
  pyCallObjectWithKeywords ff aa kk >>= wrapPtr

call3 :: (PyArg a, PyArg c, PyArg e)
      => PyObj -> a -> c -> e -> IO PyObj
call3 f a b c = withForeignPtr f $ \ fp ->
                withCString format_string $ \ format ->
                apply_arg a $ \ aa ->
                apply_arg b $ \ bb ->
                apply_arg c $ \ cc ->
                pyCallFunction3 fp format aa bb cc >>= wrapPtr
  where format_string = '(':format_char a:format_char b:format_char c:")"

class PyArg a where
  format_char :: a -> Char
  apply_arg :: a -> (PyObject -> IO x) -> IO x

instance PyArg String where
  format_char _ = 'O'
  apply_arg a f = do u <- toPyUnicode a
                     withForeignPtr u f

instance PyArg PyObj where
  format_char _ = 'O'
  apply_arg = withForeignPtr

instance PyArg CInt where
  format_char _ = 'i'
  apply_arg a f = f (wordPtrToPtr (fromIntegral a))

-------------------------------------------------------------------------------
-- Literal values
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python-local.h py_return_true"
  pyReturnTrue :: IO PyObject

-- New Reference
foreign import ccall "python-local.h py_return_false"
  pyReturnFalse :: IO PyObject

foreign import ccall "python2.5/Python.h &_Py_NoneStruct"
  pyNone :: PyObject

isNone obj = withForeignPtr obj $ \ o -> return (o == pyNone)

isNull obj = withForeignPtr obj $ \ o -> return (o == nullPtr)

returnTrue = wrapPtr =<< pyReturnTrue
returnFalse = wrapPtr =<< pyReturnFalse

-------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python-local.h py_unicode_decode_utf8"
  pyUnicodeDecodeUTF8 :: CString -> CInt -> IO PyObject

-- New Reference
foreign import ccall "python-local.h py_unicode_as_utf8"
  pyUnicodeAsUTF8 :: PyObject -> IO PyObject

toPyUnicode xs = withCStringLen (UTF8.encodeString xs) $ \ (p, len) ->
                 wrapPtr =<< pyUnicodeDecodeUTF8 p (fromIntegral len)

unicodeToString obj =
  alloca $ \ cstr_ptr ->
  withCString "s" $ \ format ->
  withForeignPtr obj $ \ r ->
  pyUnicodeAsUTF8 r >>= \ s ->
  pyParse s format cstr_ptr >>
  pyDecRef s >>
  peek cstr_ptr >>= peekCString >>= return . UTF8.decodeString

toString obj =
  alloca $ \ cstr_ptr ->
  withCString "s" $ \ format ->
  withForeignPtr obj $ \ r ->
  pyParse r format cstr_ptr >>
  peek cstr_ptr >>= peekCString >>= return . UTF8.decodeString

-------------------------------------------------------------------------------
-- Tuples
-------------------------------------------------------------------------------

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyTuple_GetItem"
  pyTupleGetItem :: PyObject -> CInt -> IO PyObject

tupleGetItem :: PyObj -> Int -> IO PyObj
tupleGetItem obj index =
  withForeignPtr obj $ \ o ->
  pyTupleGetItem o (fromIntegral index) >>= \ x ->
  pyIncRef x >>
  wrapPtr x

-------------------------------------------------------------------------------
-- Exception Handling
-------------------------------------------------------------------------------

foreign import ccall "python2.5/Python.h PyErr_Clear"
  pyErrClear :: IO ()

foreign import ccall "python2.5/Python.h PyErr_Occurred"
  pyErrOccurred :: IO PyObject

foreign import ccall "python2.5/Python.h PyErr_ExceptionMatches"
  pyErrExceptionMatches :: PyObject -> IO CInt

foreign import ccall "python2.5/Python.h &PyExc_BaseException"
  pyExcBaseException  :: PyObject

-------------------------------------------------------------------------------
-- Iterators
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyIter_Next"
  pyIterNext :: PyObject -> IO PyObject

foreign import ccall "python-local.h py_iter_check"
  pyIterCheck :: PyObject -> IO CInt

forEach_ :: PyObj -> (PyObj -> IO ()) -> IO ()
forEach_ obj f = withForeignPtr obj check
  where

  check o = do support <- pyIterCheck o
               if support == 1 then loop o
                  else fail "Iterator protocol not supported"

  loop o = do x <- pyIterNext o
              unless (x == nullPtr) (wrapPtr x >>= f >> loop o)

forEach :: PyObj -> (PyObj -> IO a) -> IO [a]
forEach obj f = withForeignPtr obj check
  where

  check o = do support <- pyIterCheck o
               if support == 1 then loop o
                  else fail "Iterator protocol not supported"

  loop o = do x <- pyIterNext o
              if x == nullPtr then return []
               else wrapPtr x >>= f >>= \ a -> loop o >>= \ as -> return (a:as)


