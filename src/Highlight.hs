{-# LANGUAGE ForeignFunctionInterface #-}
module Highlight (init_highlighter, highlight, get_languages) where

import Data.List (sortBy)
import Foreign
import Foreign.C
import Foreign.C.String

import Codec.Binary.UTF8.String

init_highlighter :: IO ()
init_highlighter =
 do pyInitialize
    pyEvalInitThreads
    runPythonFile "highlighter.py"
    pyEvalReleaseLock

highlight :: Int -> String -> String -> IO String
highlight pasteid lang code = withGIL $
  withObj' (fromImport "__main__" "hl") $ \ f ->
  withObj' (call3 f code lang pasteid ) getString

get_languages :: IO [(String,String)]
get_languages = withGIL $
  withObj' (fromImport "__main__" "get_all_lexers") $ \ get_lexers ->
  withObj' (call0 get_lexers)                       $ \ lexers     ->
  do xs <- forEach lexers $ \ x ->
      do k <- getString =<< tupleGetItem x 0
         aliases <- tupleGetItem x 1
         v <- getString =<< tupleGetItem aliases 0
         return (k,v)
     return $ sortBy (comparing fst) xs

comparing f x y = f x `compare` f y

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

-- | Pointers to new references. These are reference counted.
data PyObjectStruct1 = PyObjectStruct1
type PyObject1 = Ptr PyObjectStruct1

-- | Pointers to borrowed references. These do not need to be released.
data PyObjectStruct = PyObjectStruct
type PyObject = Ptr PyObjectStruct

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

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyImport_AddModule"
  pyImportAddModule :: CString -> IO PyObject

-- New Reference
foreign import ccall "python2.5/Python.h PyObject_GetAttrString"
  pyObjectGetAttrString :: PyObject -> CString -> IO PyObject1

fromImport :: String -> String -> IO PyObject1
fromImport module_name object_name =
 do mod <- addModule module_name
    objectGetAttr mod object_name

addModule :: String -> IO PyObject
addModule module_name = withCString module_name pyImportAddModule

objectGetAttr :: PyObject -> String -> IO PyObject1
objectGetAttr pmod object_name = withCString object_name $ pyObjectGetAttrString pmod

-------------------------------------------------------------------------------
-- Constructing Python Values
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h Py_BuildValue"
  pyBuildValue1 :: CString -> Ptr a -> IO PyObject1

-------------------------------------------------------------------------------
-- Calling Functions
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyObject_CallFunction"
  pyCallFunction0 :: PyObject -> CString -> IO PyObject1

-- New Reference
foreign import ccall "python2.5/Python.h PyObject_CallFunction"
  pyCallFunction3 :: PyObject -> CString -> CString -> CString -> CInt
                  -> IO PyObject1

call0 :: PyObject -> IO PyObject1
call0 obj = pyCallFunction0 obj nullPtr

call3 :: PyObject -> String -> String -> Int -> IO PyObject1
call3 f a b c =
  withCString "ssi" $ \ format ->
  withCString (encodeString a)     $ \ aa     ->
  withCString (encodeString b)     $ \ bb     ->
  let cc = fromIntegral c      in
  pyCallFunction3 f format aa bb cc

-------------------------------------------------------------------------------
-- Reference counting functions
-------------------------------------------------------------------------------

foreign import ccall "python-local.h py_decref"
  pyDecRef :: PyObject1 -> IO ()

withObj' :: IO PyObject1 -> (PyObject -> IO a) -> IO a
withObj' obj1 f = do o <- obj1
                     withObj o f

withObj :: PyObject1 -> (PyObject -> IO a) -> IO a
withObj obj1 f = do x <- f (castPtr obj1) -- only place the pointer is casted!
                    pyDecRef obj1
                    return x

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
  peek cstr_ptr >>= fmap decodeString . peekCString

-------------------------------------------------------------------------------
-- Iterator Protocol
-------------------------------------------------------------------------------

-- New Reference
foreign import ccall "python2.5/Python.h PyIter_Next"
  pyIterNext :: PyObject -> IO PyObject1

forEach :: PyObject -> (PyObject -> IO a) -> IO [a]
forEach obj f =
 do x <- pyIterNext obj
    if x == nullPtr
       then return []
       else do a  <- withObj x f
               as <- forEach obj f
               return (a:as)

-------------------------------------------------------------------------------
-- Tuple Protocol
-------------------------------------------------------------------------------

-- Borrowed Reference
foreign import ccall "python2.5/Python.h PyTuple_GetItem"
  pyTupleGetItem :: PyObject -> CInt -> IO PyObject

tupleGetItem :: PyObject -> Int -> IO PyObject
tupleGetItem obj index = pyTupleGetItem obj (fromIntegral index)
