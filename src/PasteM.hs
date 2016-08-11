module PasteM where

import Config
import Pages
import Storage
import Highlight
import Utils.Compat()

import Codec.Binary.UTF8.String as UTF8
import Data.Typeable
import MonadLib
import Network.CGI
import Text.XHtml.Strict hiding (URL)

newtype PasteM a = PasteM { unPasteM :: ReaderT PasteEnv (CGIT IO) a }
  deriving (Monad, Applicative, Functor, MonadCGI, MonadIO)

type Action = PasteM CGIResult

data PasteEnv = PasteEnv
       { env_python_handle :: PythonHandle
       , env_config        :: Config
       }

withConf :: (Config -> a) -> PasteM a
withConf f = fmap f getConf

getConf :: PasteM Config
getConf = fmap env_config (PasteM ask)

exec_python :: PythonM a -> PasteM a
exec_python m =
 do h <- fmap env_python_handle (PasteM ask)
    liftIO $ runPythonM h m

runPasteM :: PythonHandle -> Config -> PasteM a -> CGI a
runPasteM h c m = runReaderT (PasteEnv h c) $ unPasteM m

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
