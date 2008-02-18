{-# LANGUAGE FunctionalDependencies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances,
             UndecidableInstances,
             EmptyDataDecls
             #-}

module Utils.RequestDispatch
         ( Context(..), Method(..), IsArg(..), Arg(..)
         , Req, Opt, Many, (:>)(..)
         , (-->), runAPI, methodURL, methodURLBase) where

import MonadLib
import qualified Data.ByteString as BS

import Utils.URL
import Utils.Misc

data Arg a c = Arg String            -- ^ Arg Name
data Many
data Req
data Opt

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

(-->) :: (ArgsDoc t, Apply t f c)
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
  { cMethod :: String
  , cPath   :: String
  , cParams :: [(String,String)]
  } deriving (Show)

makeDispatcher :: Apply t f c => Method t -> f -> Context -> Maybe (Error c)
makeDispatcher m handler p = do
  guard $ cPath p == mName m && cMethod p == mHTTPMethod m
  return $ apply (cParams p) (mArguments m) handler

methodURLBase :: Method args -> URL
methodURLBase (Method _ _ path _) = baseURL path

methodURL :: Build args f => Method args -> f
methodURL (Method args _ path _) = build args $ baseURL path


baseURL path =
  URL { url_type     = HostRelative
      , url_path     = path
      , url_params   = []
      }


class Build a b | a -> b where
  build :: a -> URL -> b

instance Build () URL where
  build _ xs                    = xs

instance (IsArg a, Build b c) =>
           Build (Arg Many a :> b) ([(String,a)] -> c) where
  build (Arg name :> b) url xs  = build b (foldl mk url xs)
    where mk url (s,val) = add_param url (name ++ "__" ++ s,show_arg val)

instance (IsArg a, Build b c) => Build (Arg Req a :> b) (a -> c) where
  build (Arg name :> b) url val = build b $ add_param url (name,show_arg val)

instance (IsArg a, Build b c) =>
            Build (Arg Opt a :> b) (Maybe a -> c) where
  build (Arg name :> b) url mv  = build b $ case mv of
                                    Just v  -> url `add_param` (name,show_arg v)
                                    Nothing -> url

instance Build b c => Build (Arg x a :> b) c where
  build (_ :> b)                = build  b




type ApplyCtx = [(String,String)]

class Apply t f z | t f -> z where
  apply                         :: ApplyCtx -> t -> f -> Error z

instance Apply () f f where
  apply _ _                     = return

instance ( ReadLogic arg f
         , Apply t r z )
         => Apply (arg :> t) (f -> r) z where
  apply ctx (arg :> b) f        = do u <- read_logic arg ctx
                                     apply ctx b (f u)





-- | Lookup an argument, returns Nothing if the argument was not present, and Just
-- Nothing if the argument couldn't be parsed
lookup_arg :: IsArg a => ApplyCtx -> String -> Maybe (Maybe a)
lookup_arg m k = readArg `fmap` lookup k m


class ReadLogic a c | a -> c where
  read_logic                    :: a -> ApplyCtx -> Error c
  logic_doc                     :: a -> ShowS

instance IsArg t => ReadLogic (Arg Req t) t where
  read_logic (Arg n) ctx = case lookup_arg ctx n of
    Just (Just x)       -> return x
    Nothing             -> err $ "Required argument missing: " ++ n
    Just Nothing        -> err $ "Failed to parse: " ++ n
  logic_doc _ xs                = xs

instance IsArg t => ReadLogic (Arg Opt t) (Maybe t) where
  read_logic (Arg n) ctx  = case lookup_arg ctx n of
    Just (Just v)       -> return (Just v)
    Nothing             -> return Nothing
    Just Nothing        -> err $ "Failed to parse: " ++ n
  logic_doc _                   = showString "Optional "

-- | An alternative to Read, for parsing url values
class IsArg a where
  show_arg                      :: a -> String
  arg_doc                       :: b a -> ShowS
  readArg                       :: String -> Maybe a

instance IsArg Int where
  readArg                    = maybeRead
  show_arg                      = show
  arg_doc _                     = showString "Int"

instance IsArg String where
  readArg                    = return
  show_arg xs                   = xs
  arg_doc _                     = showString "String"

instance IsArg BS.ByteString where
  readArg                       = return . utf8_encode
  show_arg                      = utf8_decode
  arg_doc _                     = showString "String"


class    ArgsDoc a  where args_doc      :: a -> ShowS
instance ArgsDoc () where args_doc _ xs = xs
instance ( IsArg a
         , ArgsDoc b
         , ReadLogic (Arg x a) k )
         => ArgsDoc (Arg x a :> b) where
  args_doc (a@(Arg n) :> b)     = showString n . showString " : "
                                . logic_doc a
                                . arg_doc a    . showString "\n"
                                . args_doc b



type Error = ExceptionT String Id
runError = runId . runExceptionT
err x = raise x

