{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}
module Python where

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


foreign import ccall "python2.5/Python.h PyEval_InitThreads"
  pyEvalInitThreads :: IO ()
foreign import ccall "python2.5/Python.h PyEval_ReleaseLock"
  pyEvalReleaseLock :: IO ()
foreign import ccall "python2.5/Python.h PyGILState_Ensure"
  pyGILStateEnsure :: IO (Ptr ())
foreign import ccall "python2.5/Python.h PyGILState_Release"
  pyGILStateRelease :: Ptr () -> IO ()

withGIL m = do s <- pyGILStateEnsure
               x <- m
               pyGILStateRelease s
               return x

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

dictGetItem d k =
  pyDictGetItem d k >>= \ x ->
  pyIncRef x >>
  return x

dictSetItemString d k v =
  withCString    k $ \ kk ->
  pyDictSetItemString d kk v

dictGetItemString d k =
  withCString    k $ \ kk ->
  pyDictGetItemString d kk >>= \ x ->
  pyIncRef x >>
  return x

dictFromList xs =
 do d <- pyDictNew
    mapM_ (aux d) xs
    return d
 where
 aux d (k, v) =
   withCString k $ \ kk ->
   pyDictSetItemString d kk v



importModule :: String -> IO PyObject
importModule module_name = withCString module_name pyImportImportModule

fetch :: PyObject -> String -> IO PyObject
fetch pmod object_name = withCString object_name $ pyObjectGetAttrString pmod

buildValue1 pat val =
  withCString pat $ \ ppat ->
  apply_arg val $ \ pval ->
  pyBuildValue1 ppat pval

buildValue3 pat a b c =
  withCString pat $ \ ppat ->
  apply_arg a     $ \ aa   ->
  apply_arg b     $ \ bb   ->
  apply_arg c     $ \ cc   ->
  pyBuildValue3 ppat aa bb cc

fromImport mod klass =
 do pmod <- importModule mod
    obj <- fetch pmod klass
    pyDecRef pmod
    return obj

-- from pygments import highlight
-- from pygments.lexers import PythonLexer
-- from pygments.formatters import HtmlFormatter
-- code = 'asdf'
-- lexer = get_lexer_by_name("python", stripall=True)
-- formatter = HtmlFormatter(linenos=True, cssclass="source")
-- result = highlight(code, lexer, formatter)
--

make_highlighter :: IO (Int -> String -> String -> IO String)
make_highlighter = return $ \ pasteid lang code -> withGIL $
  do f <- fromImport "__main__" "hl"
     r <- call3 f code lang (show pasteid)
     unicodeToString r

{-
make_highlighter :: IO (Int -> String -> String -> IO String)
make_highlighter = withGIL $
 do t <- pyReturnTrue
    get_lexer <- fromImport "pygments.lexers" "get_lexer_by_name"
    -- formatter_class <- fromImport "pygments.formatters" "HtmlFormatter"
    formatter_class <- fromImport "__main__" "HtmlLineFormatter"
    line_anchor_str <- withCString "s" $ \ form -> withCString "li" $ pyBuildValue1 form
    formatter <- call0withKeywords formatter_class [("linenos", t),("lineanchors",line_anchor_str)]
    pyDecRef line_anchor_str
    pyDecRef formatter_class
    highlight <- fromImport "pygments" "highlight"
    let lexer_dict = [("stripall",t)]
    return $ \ pasteid lang code -> withGIL $
      do p <- withCString "s" $ \ format -> withCString (show pasteid) (pyBuildValue1 format)
         lexer <- call1withKeywords get_lexer lang (("pasteid",p):lexer_dict)
         pyDecRef p
         if isNull lexer
           then return "no lexer"
           else unicodeToString =<< call3 highlight code lexer formatter
-}

get_languages :: IO [(String,String)]
get_languages = withGIL $
 do lexers <- call0 =<< fromImport "pygments.lexers" "get_all_lexers"
    forEach lexers $ \ x -> do k <- tupleGetItem x 0 >>= toString
                               aliases <- tupleGetItem x 1
                               v <- toString =<< tupleGetItem aliases 0
                               return (k,v)

withPython m = bracket_ pyInitialize pyFinalize m

main = withPython $ do highlightAs <- make_highlighter
                       highlightAs 1 "haskell" "sdfasdf"
                       highlightAs 2 "skell" "asdf"
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

call0 f = withCString "()" $ \ format ->
          pyCallFunction0 f format

call0withKeywords :: PyObject -> [(String,PyObject)] -> IO PyObject
call0withKeywords f ks =
 do k <- dictFromList ks
    x <- pyCallObjectWithKeywords f nullPtr k
    pyDecRef k
    return x

callMethod0 obj meth =
  withCString meth   $ \ m ->
  withCString "()"   $ \ format ->
  pyCallMethod0 obj m format

call1 f a = withCString ('(':format_char a:")") $ \ format ->
            apply_arg a $ \ aa ->
            pyCallFunction1 f format aa

call1withKeywords f a ks =
 do k <- dictFromList ks
    args <- buildValue1 ('(':format_char a:")") a
    x <- pyCallObjectWithKeywords f args k
    pyDecRef args
    pyDecRef k
    return x

call3 f a b c = withCString format_string $ \ format ->
                apply_arg a $ \ aa ->
                apply_arg b $ \ bb ->
                apply_arg c $ \ cc ->
                pyCallFunction3 f format aa bb cc
  where format_string = '(':format_char a:format_char b:format_char c:")"

class PyArg a where
  format_char :: a -> Char
  apply_arg :: a -> (PyObject -> IO x) -> IO x

instance PyArg String where
  format_char _ = 'O'
  apply_arg a f = do u <- toPyUnicode a
                     x <- f u
                     pyDecRef u
                     return x

instance PyArg PyObject where
  format_char _ = 'O'
  apply_arg a f = f a

instance PyArg Int where
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

isNone obj = obj == pyNone

isNull obj = obj == nullPtr

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
                 pyUnicodeDecodeUTF8 p (fromIntegral len)

unicodeToString obj =
  alloca $ \ cstr_ptr ->
  withCString "s" $ \ format ->
  pyUnicodeAsUTF8 obj >>= \ s ->
  pyParse s format cstr_ptr >>
  pyDecRef s >>
  peek cstr_ptr >>= peekCString >>= return . UTF8.decodeString

toString obj =
  alloca $ \ cstr_ptr ->
  withCString "s" $ \ format ->
  pyParse obj format cstr_ptr >>
  peek cstr_ptr >>= peekCString >>= return . UTF8.decodeString

-------------------------------------------------------------------------------
-- Tuples
-------------------------------------------------------------------------------

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyTuple_GetItem"
  pyTupleGetItem :: PyObject -> CInt -> IO PyObject

tupleGetItem obj index =
  pyTupleGetItem obj (fromIntegral index)

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

forEach_ obj f = check obj
  where

  check o = do support <- pyIterCheck o
               if support == 1 then loop o
                  else fail "Iterator protocol not supported"

  loop o = do x <- pyIterNext o
              unless (x == nullPtr) (f x >> loop o)

forEach obj f = check obj
  where

  check o = do support <- pyIterCheck o
               if support == 1 then loop o
                  else fail "Iterator protocol not supported"

  loop o = do x <- pyIterNext o
              if x == nullPtr then return []
               else f x >>= \ a -> loop o >>= \ as -> return (a:as)


