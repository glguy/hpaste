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
         ( Context, Method(..), IsArg(..), Arg(..)
         , Req, Opt, Many, FArg, UArg, (:>)(..)
         , (-->), runAPI, methodURL, methodURLBase) where

import Data.List (isPrefixOf)
import MonadLib
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BS

import Network.HTTP
import Utils.URL
import Utils.Misc

type Context = (URL, Request)

data Arg a b c = Arg String            -- ^ Arg Name
data Many
data Req
data Opt
data UArg
data FArg

data Method a = Method a RequestMethod String String

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

makeDispatcher :: Apply t f c => Method t -> f -> Context -> Maybe (Error c)
makeDispatcher (Method args method name _) f (url,req) = do
  guard $ url_path url == name && rqMethod req == method
  return $ apply (url_params url,posted_form req) args f

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
           Build (Arg Many UArg a :> b) ([(String,a)] -> c) where
  build (Arg name :> b) url xs  = build b (foldl mk url xs)
    where mk url (s,val) = add_param url (name ++ "__" ++ s,show_arg val)

instance (IsArg a, Build b c) => Build (Arg Req UArg a :> b) (a -> c) where
  build (Arg name :> b) url val = build b $ add_param url (name,show_arg val)

instance (IsArg a, Build b c) =>
            Build (Arg Opt UArg a :> b) (Maybe a -> c) where
  build (Arg name :> b) url mv  = build b $ case mv of
                                    Just v  -> url `add_param` (name,show_arg v)
                                    Nothing -> url

instance Build b c => Build (Arg x FArg a :> b) c where
  build (_ :> b)                = build  b




type ApplyCtx = ([(String,String)],Maybe FormFields)

class Apply t f z | t f -> z where
  apply                         :: ApplyCtx -> t -> f -> Error z

instance Apply () f f where
  apply _ _                     = return

instance ( ReadLogic arg a f
         , Apply t r z )
         => Apply (arg :> t) (f -> r) z where
  apply ctx (arg :> b) f        = do u <- read_logic arg ctx
                                     apply ctx b (f u)



class IsArg b => SourceLogic a b | a -> b where
  source_logic                  :: a -> ApplyCtx -> String -> Maybe (Maybe b)
  source_doc                    :: a -> ShowS
  source_names                  :: a -> ApplyCtx -> [String]


instance IsArg t => SourceLogic (Arg r UArg t) t where
  source_logic _ (m,_) k        = readUrlArg `fmap` lookup k m
  source_doc _ xs               = xs
  source_names (Arg k) (m,_)    = [x | (x,_) <- m, (k ++ "__") `isPrefixOf` x]

instance IsArg t => SourceLogic (Arg r FArg t) t where
  source_logic _ (_,m) k        = fmap readFormArg . (`lookup_bytes` k) =<< m
  source_doc _                  = showString "Posted "
  source_names (Arg k) (_,m)    = filter ((k ++ "__") `isPrefixOf`)
                                         (maybe [] formfield_names m)



class SourceLogic a b => ReadLogic a b c | a -> b c where
  read_logic                    :: a -> ApplyCtx -> Error c
  logic_doc                     :: a -> ShowS

instance SourceLogic (Arg Req s t) t => ReadLogic (Arg Req s t) t t where
  read_logic a@(Arg n) ctx = case source_logic a ctx n of
    Just (Just x)       -> return x
    Nothing             -> err $ "Required argument missing: " ++ n
    Just Nothing        -> err $ "Failed to parse: " ++ n
  logic_doc _ xs                = xs

instance SourceLogic (Arg Opt s t) t =>
           ReadLogic (Arg Opt s t) t (Maybe t) where
  read_logic a@(Arg n) ctx  = case source_logic a ctx n of
    Just (Just v)       -> return (Just v)
    Nothing             -> return Nothing
    Just Nothing        -> err $ "Failed to parse: " ++ n
  logic_doc _                   = showString "Optional "

instance SourceLogic (Arg Many s t) t =>
           ReadLogic (Arg Many s t) t [(String,t)] where
  read_logic a@(Arg n) ctx =
    let names = source_names a ctx in
    case sequence $ fromJust $ mapM (source_logic a ctx) names of
      Just xs -> return $ zip (map (drop (length n + 2)) names) xs
      Nothing -> err $ "Failed while parsing: " ++ n
  logic_doc _                   = showString "Many "

class IsArg a where
  show_arg                      :: a -> String
  arg_doc                       :: b a -> ShowS
  readUrlArg                    :: String -> Maybe a
  readFormArg                   :: BS.ByteString -> Maybe a
  readFormArg                   = readUrlArg . utf8_decode

instance IsArg Int where
  readUrlArg                    = maybeRead
  show_arg                      = show
  arg_doc _                     = showString "Int"

instance IsArg String where
  readUrlArg                    = return
  show_arg xs                   = xs
  arg_doc _                     = showString "String"

instance IsArg BS.ByteString where
  readFormArg                   = return
  readUrlArg                    = return . utf8_encode
  show_arg                      = utf8_decode
  arg_doc _                     = showString "String"


class    ArgsDoc a  where args_doc      :: a -> ShowS
instance ArgsDoc () where args_doc _ xs = xs
instance ( IsArg a
         , ArgsDoc b
         , SourceLogic (Arg x y a) z, ReadLogic (Arg x y a) j k )
         => ArgsDoc (Arg x y a :> b) where
  args_doc (a@(Arg n) :> b)     = showString n . showString " : "
                                . source_doc a . logic_doc a
                                . arg_doc a    . showString "\n"
                                . args_doc b



type Error = ExceptionT String Id
runError = runId . runExceptionT
err x = raise x

