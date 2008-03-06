{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module Highlight
         ( PythonHandle(), PythonM()
         , init_highlighter, highlight, get_languages, runPythonM) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.List (sortBy)
import Foreign
import Foreign.C
import Foreign.C.String

import Data.ByteString (packCStringLen, useAsCString)
import Data.ByteString.UTF8 as UTF8

newtype PythonHandle = PythonHandle (MVar ())
newtype PythonM a = PythonM (IO a)
  deriving (Monad,Functor)

runPythonM (PythonHandle qsem) (PythonM m) =
 do liftIO $ takeMVar qsem
    x <- m
    liftIO $ putMVar qsem ()
    return x

init_highlighter :: IO PythonHandle
init_highlighter =
 do pyInitialize
    runPythonFile "highlighter.py"
    PythonHandle `fmap` newMVar ()

highlight :: Int -> String -> String -> PythonM String
highlight pasteid lang code = PythonM $
  withObj' (fromImport "__main__" "hl") $ \ f ->
  withObj' (call3 f code lang pasteid ) getString

get_languages :: PythonM [(String,String)]
get_languages = PythonM $
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

-- | Pointers to borrowed references. These do not need to be released.
data PyObjectStruct
type PyObject = Ptr PyObjectStruct

data PyObjectStruct1
type PyObject1 = Ptr PyObjectStruct1

foreign import ccall "python2.5/Python.h Py_Initialize"
  pyInitialize :: IO ()

foreign import ccall "python2.5/Python.h Py_Finalize"
  pyFinalize :: IO ()


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
  useAsCString (UTF8.fromString a) $ \ aa     ->
  useAsCString (UTF8.fromString b) $ \ bb     ->
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

foreign import ccall "python2.5/Python.h PyString_AsStringAndSize"
  pyStringAsStringAndSize :: PyObject -> Ptr CString -> Ptr CInt -> IO CInt

getString :: PyObject -> IO String
getString obj =
  alloca $ \ cstr_ptr ->
  alloca $ \ len_ptr ->
  do pyStringAsStringAndSize obj cstr_ptr len_ptr
     cstr <- peek cstr_ptr
     len  <- peek len_ptr
     UTF8.toString `fmap` packCStringLen (cstr,fromIntegral len)

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
