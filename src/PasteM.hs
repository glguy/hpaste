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

get_conf :: PasteM Config
get_conf = fmap env_config (PasteM ask)

exec_python :: PythonM a -> PasteM a
exec_python m =
 do h <- fmap env_python_handle (PasteM ask)
    liftIO $ runPythonM h m

ask_session_id :: PasteM SessionId
ask_session_id = fmap env_session_id (PasteM ask)

runPasteM :: SessionId -> PythonHandle -> Config
          -> PasteM a -> CGI a
runPasteM a c d =
  runReaderT (PasteEnv a c d) . evalStateT emptyState . unPasteM

evalStateT s m = fmap fst $ runStateT s m

session_cookie_name :: String
session_cookie_name = "sid"

get_session_data = PasteM (state_session_data `fmap` get)

modify_session_data f =
  do st <- PasteM get
     PasteM $ set $ st { state_session_data = f (state_session_data st) }

load_session_data :: PasteM ()
load_session_data =
 do sid <- ask_session_id
    dat <- exec_db (retrieveSessionData sid)
    modify_session_data (const dat)

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
  , cookiePath = Nothing
  , cookieSecure = False
  }


-------------------------------------------------------------------------------
-- Lifting functions for auxillary monads into PasteM
-------------------------------------------------------------------------------

-- | Lift a StoreM computation into the PasteM monad.
exec_db :: Typeable a => StoreM a -> PasteM a
exec_db m = do path <- db_path `fmap` get_conf
               liftIO (runStoreM path m)

-- | Lift a PageM computation into the PasteM monad and output the result.
outputHTML :: HTML a => PageM a -> PasteM CGIResult
outputHTML s = do setHeader "Content-type" "text/html; charset=utf-8"
                  xs <- buildHTML s
                  output $ UTF8.encodeString $ showHtml xs
  where
  buildHTML :: PageM a -> PasteM a
  buildHTML m      = do sn <- scriptName
                        conf <- get_conf
                        return $ runPageM conf sn m

