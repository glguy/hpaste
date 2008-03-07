module PasteM where

import Config
import Types
import Pages
import Storage
import Highlight
import Utils.Misc
import Utils.Compat()

import Codec.Binary.UTF8.String as UTF8
import Data.Typeable
import MonadLib
import Network.CGI
import System.Random (randomIO)
import Text.XHtml.Strict hiding (URL)

newtype PasteM a = PasteM { unPasteM :: StateT PasteState (ReaderT PasteEnv (CGIT IO)) a }
  deriving (Monad, Functor, MonadCGI, MonadIO)

type Action = PasteM CGIResult

data PasteEnv = PasteEnv
       { env_session_id    :: SessionId
       , env_python_handle :: PythonHandle
       , env_config        :: Config
       }

data PasteState = PasteState
       { state_session_data :: SessionData }

emptyState = PasteState emptySessionData

withConf :: (Config -> a) -> PasteM a
withConf f = fmap f getConf

getConf :: PasteM Config
getConf = fmap env_config (PasteM ask)

exec_python :: PythonM a -> PasteM a
exec_python m =
 do h <- fmap env_python_handle (PasteM ask)
    liftIO $ runPythonM h m

ask_session_id :: PasteM SessionId
ask_session_id = fmap env_session_id (PasteM ask)

runPasteM :: PythonHandle -> Config -> PasteM a -> CGI a
runPasteM h c m =
 do sid <- get_session_id
    dat <- liftIO $ runStoreM (db_path c) $ retrieveSessionData sid
    (r,dat') <- runReaderT (PasteEnv sid h c)
              $ runStateT (PasteState dat)
              $ unPasteM m
    storeSessionData sid dat'
    return r

session_cookie_name :: String
session_cookie_name = "sid"

get_session_data = PasteM (state_session_data `fmap` get)

modify_session_data f =
  do st <- PasteM get
     PasteM $ set $ st { state_session_data = f (state_session_data st) }

save_session_data :: PasteM ()
save_session_data =
 do sid <- ask_session_id
    dat <- get_session_data
    exec_db (storeSessionData sid dat)

get_session_id :: (MonadIO m, MonadCGI m) => m SessionId
get_session_id =
 do mb_sessionCookie <- getCookie session_cookie_name
    case maybeRead =<< mb_sessionCookie of
      Nothing -> do sid <- liftIO generate_sid
                    setCookie (make_session_cookie sid)
                    return sid
      Just sid -> return sid

generate_sid :: IO Int
generate_sid = randomIO

make_session_cookie :: Int -> Cookie
make_session_cookie sid = Cookie
  { cookieName = session_cookie_name
  , cookieValue = show sid
  , cookieExpires = Nothing
  , cookieDomain = Nothing
  , cookiePath = Just "/"
  , cookieSecure = False
  }


-------------------------------------------------------------------------------
-- Lifting functions for auxillary monads into PasteM
-------------------------------------------------------------------------------

-- | Lift a StoreM computation into the PasteM monad.
exec_db :: Typeable a => StoreM a -> PasteM a
exec_db m = do path <- withConf db_path
               liftIO (runStoreM path m)

-- | Lift a PageM computation into the PasteM monad and output the result.
outputHTML :: HTML a => PageM a -> PasteM CGIResult
outputHTML s = do setContentType html_content_type
                  output . UTF8.encodeString . showHtml =<< buildHTML s
  where
  buildHTML :: PageM a -> PasteM a
  buildHTML m = do c <- getConf
                   sn <- scriptName
                   return $ runPageM c sn m

setContentType = setHeader "Content-type" . showContentType
css_content_type =
  ContentType { ctType = "text", ctSubtype = "css" , ctParameters = utf8_ct }
html_content_type =
  ContentType { ctType = "text", ctSubtype = "html" , ctParameters = utf8_ct }
plain_content_type =
  ContentType { ctType = "text", ctSubtype = "plain" , ctParameters = utf8_ct }
utf8_ct = [("charset","utf-8")]
