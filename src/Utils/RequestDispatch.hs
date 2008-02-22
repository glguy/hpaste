{-# LANGUAGE FunctionalDependencies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             TypeSynonymInstances,
             UndecidableInstances,
             EmptyDataDecls
             #-}

module Utils.RequestDispatch
         ( Context(..), Method(..), IsArg(..), Arg(..)
         , Req, Opt, (:>)(..)
         , (-->), runAPI, methodURL, methodURLBase) where

import MonadLib
import qualified Data.ByteString as BS
import Data.Maybe(mapMaybe)

import Utils.URL
import Utils.Misc

data Arg a c = Arg String            -- ^ Arg Name
data Req
data Opt
data Many

data Method a = Method
  { mArguments  :: a
  , mHTTPMethod :: String
  , mName       :: String
  , mDocString  :: String
  } deriving (Show)

data a :> b = a :> b
  deriving Show

infixr :>

runAPI :: Context
       -> [Context -> Maybe (Either String a)]
       -> Maybe (Either String a)
runAPI ctx handlers = msum $ map ($ ctx) handlers

(-->) :: (ArgsDoc t, Handler t f c)
      => Method t
      -> f
      -> (Context -> Maybe (Either String c), String)
x --> y = (\ctx -> (f . runError) `fmap` makeDispatcher x y ctx, docMethod x)
  where
  f (Left  e) = Left  $ e ++ "\n\n" ++ docMethod x
  f (Right v) = Right v

docMethod :: ArgsDoc a => Method a -> String
docMethod (Method args method name docString) =
  showString docString $ showString "\n" $ shows method $
  showString " / "     $ showString name $ showString "\n" $ args_doc args ""


data Context = Context
  { cMethod :: String       -- ^ HTTP method
  , cPath   :: String
  , cParams :: [(String,String)]
  } deriving (Show)

makeDispatcher :: Handler t f c => Method t -> f -> Context -> Maybe (Error c)
makeDispatcher m handler p = do
  guard $ cPath p == mName m && cMethod p == mHTTPMethod m
  return $ apply (cParams p) (mArguments m) handler

methodURLBase :: Method args -> URL
methodURLBase (Method _ _ path _) = baseURL path

methodURL :: Build args f => Method args -> f
methodURL (Method args _ path _) = build args $ baseURL path

baseURL :: String -> URL
baseURL path =
  URL { url_type     = PathRelative
      , url_path     = path
      , url_params   = []
      }

-- Processing lists of arguments -----------------------------------------------

-- | Add a list of arguments to the query of a URL.
class Build a b | a -> b where
  build :: a -> URL -> b

instance Build () URL where build _ xs = xs
instance (Argument r a c) => Build (Arg r a) (c -> URL) where
  build a url val = foldl add_param url (arg_out a val)

instance (Argument r a c, Build args fun)
  => Build (Arg r a :> args) (c -> fun) where
  build (a :> as) url val = build as (build a url val)


-- | Generate the documentation for a list of arguments.
class    ArgsDoc a  where args_doc      :: a -> ShowS
instance ArgsDoc () where args_doc _ xs = xs
instance (Argument r a c) => ArgsDoc (Arg r a) where args_doc a = arg_doc a
instance (Argument x a k, ArgsDoc args) => ArgsDoc (Arg x a :> args) where
  args_doc (a :> b) = arg_doc a . showChar '\n' . args_doc b




type Args = [(String,String)]

class Handler ty_sig handler result | ty_sig handler -> result where
  apply :: Args -> ty_sig -> handler -> Error result

-- | Useful for methods with no arguments.
instance Handler () handler handler where
  apply _ _ h = return h

-- | Useful to avoid having to add @()@ to the end of methods with arguments.
instance (Argument r a c) => Handler (Arg r a) (c -> b) b where
  apply ctxt arg h = h `fmap` arg_in arg ctxt

instance (Argument r a f, Handler args handler result)
  => Handler (Arg r a :> args) (f -> handler) result where
  apply ctx (arg :> b) f    = do u <- arg_in arg ctx
                                 apply ctx b (f u)



--------------------------------------------------------------------------------

-- NOTE: We could use a transformer here, which would enable us
-- to support situations where accessing arguments happens in a monad.
-- (e.g., getEnv)
type Error = ExceptionT String Id
runError :: Error a -> Either String a
runError = runId . runExceptionT

err :: String -> Error a
err x = raise x

parse_arg :: IsArg t => String -> String -> Error t
parse_arg n txt = case read_arg txt of
                    Nothing -> err $ "Failed to parse: " ++ n
                    Just v  -> return v

arg_sig :: IsArg t => Arg r t -> ShowS
arg_sig a@(Arg n) = showString n . showString " : " . showString (show_type a)

class IsArg a => Argument r a c | r a -> c where
  arg_in  :: Arg r a -> Args -> Error c
  arg_out :: Arg r a -> c -> Args
  arg_doc :: Arg r a -> ShowS

instance IsArg t => Argument Req t t where
  arg_in (Arg n) ctx = case lookup n ctx of
    Nothing  -> err $ "Required argument missing: " ++ n
    Just txt -> parse_arg n txt

  arg_out (Arg n) v = [(n, show_arg v "")]

  arg_doc a = arg_sig a

instance IsArg t => Argument Opt t (Maybe t) where
  arg_in (Arg n) ctx  = case lookup n ctx of
    Nothing  -> return Nothing
    Just txt -> Just `fmap` parse_arg n txt

  arg_out _ Nothing         = []
  arg_out (Arg n) (Just v)  = [(n,show_arg v "")]

  arg_doc a = showString "Optional " . arg_sig a

instance IsArg t => Argument Many t [(String,t)] where
  arg_in (Arg n) ctx  = mapM check2 (mapMaybe check1 ctx)
    where check1 (x,y) = do k <- drop_prefix (n ++ ".") x
                            return (k,y)
          check2 (x,y)  = case read_arg y of
                            Nothing -> err $ "Failed to parse field "
                                              ++ show x ++ " of " ++ show n
                            Just v  -> return (x,v)
  -- NOTE: This assumes that the name does not contain "."
  arg_out (Arg n) vs  = [ (n ++ "." ++ x, show_arg y "") | (x,y) <- vs ]
  arg_doc a           = showString "Many " . arg_sig a


-- | Haskell types that can be used in method arguments.
class IsArg a where
  show_arg  :: a -> ShowS
  read_arg  :: String -> Maybe a
  show_type :: f a -> String

instance IsArg Int where
  read_arg    = maybeRead
  show_arg    = shows
  show_type _ = "Int"

instance IsArg String where
  read_arg    = return
  show_arg    = showString
  show_type _ = "String"

instance IsArg BS.ByteString where
  read_arg    = return . utf8_encode
  show_arg    = showString . utf8_decode
  show_type _ = "String"

instance IsArg () where
  read_arg _  = return ()
  show_arg _  = showString "1"
  show_type _ = "Anything"


