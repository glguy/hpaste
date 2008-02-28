{-# LANGUAGE ForeignFunctionInterface #-}
module Highlight (init_highlighter, highlight, get_languages) where

import Foreign
import Foreign.C
import Foreign.C.String

init_highlighter :: IO ()
init_highlighter =
 do pyInitialize
    pyEvalInitThreads
    runPythonFile "highlighter.py"
    pyEvalReleaseLock

highlight :: Int -> String -> String -> IO String
highlight pasteid lang code = withGIL $
 do f <- fromImport "__main__" "hl"
    r <- call3 f code lang pasteid
    s <- getString r
    pyDecRef r
    pyDecRef f
    return s

get_languages :: IO [(String,String)]
get_languages = withGIL $
 do lexers <- call0 =<< fromImport "pygments.lexers" "get_all_lexers"
    forEach lexers $ \ x ->
     do k <- getString =<< tupleGetItem x 0
        aliases <- tupleGetItem x 1
        v <- getString =<< tupleGetItem aliases 0
        return (k,v)

runPythonFile name = runSimpleString =<< readFile name


-------------------------------------------------------------------------------
-- Very high level Python interface
-------------------------------------------------------------------------------

foreign import ccall "python2.5/Python.h PyRun_SimpleString"
  pyRunSimpleString :: CString -> IO ()

runSimpleString code = withCString code pyRunSimpleString

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

foreign import ccall "python2.5/Python.h Py_Initialize"
  pyInitialize :: IO ()

foreign import ccall "python2.5/Python.h Py_Finalize"
  pyFinalize :: IO ()

-------------------------------------------------------------------------------
-- Global Interpreter Lock Functions
-------------------------------------------------------------------------------

data PyGILStateStateStruct = PyGILStateStateStruct
type PyGILStateState = Ptr PyGILStateStateStruct

foreign import ccall "python2.5/Python.h PyEval_InitThreads"
  pyEvalInitThreads :: IO ()

foreign import ccall "python2.5/Python.h PyGILState_Ensure"
  pyGILStateEnsure :: IO PyGILStateState

foreign import ccall "python2.5/Python.h PyGILState_Release"
  pyGILStateRelease :: PyGILStateState -> IO ()

foreign import ccall "python2.5/Python.h PyEval_ReleaseLock"
  pyEvalReleaseLock :: IO ()

withGIL :: IO a -> IO a
withGIL m =
 do s <- pyGILStateEnsure
    x <- m
    pyGILStateRelease s
    return x

-------------------------------------------------------------------------------
-- Accessing objects from namespaces
-------------------------------------------------------------------------------

data PyObjectStruct = PyObjectStruct
type PyObject = Ptr PyObjectStruct

foreign import ccall "python2.5/Python.h PyImport_ImportModule"
  pyImportImportModule :: CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyObject_GetAttrString"
  pyObjectGetAttrString :: PyObject -> CString -> IO PyObject

fromImport :: String -> String -> IO PyObject
fromImport module_name object_name =
 do mod <- importModule module_name
    obj <- objectGetAttr mod object_name
    pyDecRef mod
    return obj

importModule :: String -> IO PyObject
importModule module_name = withCString module_name pyImportImportModule

objectGetAttr :: PyObject -> String -> IO PyObject
objectGetAttr pmod object_name = withCString object_name $ pyObjectGetAttrString pmod

-------------------------------------------------------------------------------
-- Constructing Python Values
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue1 :: CString -> Ptr a -> IO PyObject

-------------------------------------------------------------------------------
-- Calling Functions
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction0 :: PyObject -> CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyEval_CallFunction"
  pyCallFunction3 :: PyObject -> CString -> CString -> CString -> CInt -> IO PyObject

call0 :: PyObject -> IO PyObject
call0 obj = pyCallFunction0 obj nullPtr

call3 :: PyObject -> String -> String -> Int -> IO PyObject
call3 f a b c =
  withCString "(ssi)" $ \ format ->
  withCString a     $ \ aa     ->
  withCString b     $ \ bb     ->
  let cc = fromIntegral c      in
  pyCallFunction3 f format aa bb cc

-------------------------------------------------------------------------------
-- Reference counting functions
-------------------------------------------------------------------------------

foreign import ccall "python-local.h py_decref"
  pyDecRef :: PyObject -> IO ()

-------------------------------------------------------------------------------
-- String Extraction
-------------------------------------------------------------------------------

foreign import ccall "python2.5/Python.h PyArg_Parse"
  pyArgParse :: PyObject -> CString -> Ptr CString -> IO CInt

getString :: PyObject -> IO String
getString obj =
  withCString "s" $ \ format   ->
  alloca          $ \ cstr_ptr ->
  pyArgParse obj format cstr_ptr >>
  peek cstr_ptr >>= peekCString

-------------------------------------------------------------------------------
-- Iterator Protocol
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyIter_Next"
  pyIterNext :: PyObject -> IO PyObject

forEach obj f =
 do x <- pyIterNext obj
    if x == nullPtr
       then return []
       else do a  <- f x
               as <- forEach obj f
               return (a:as)

-------------------------------------------------------------------------------
-- Tuple Protocol
-------------------------------------------------------------------------------

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyTuple_GetItem"
  pyTupleGetItem :: PyObject -> CInt -> IO PyObject

tupleGetItem obj index = pyTupleGetItem obj (fromIntegral index)
