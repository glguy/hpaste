module Utils.Compat where

import MonadLib
import Network.CGI.Monad
import qualified Control.Monad.Trans as MTL

instance MonadT CGIT where
  lift = MTL.lift

instance BaseM m n => BaseM (CGIT m) n where
  inBase = lift . inBase

instance MonadCGI m => MonadCGI (ReaderT r m) where
  cgiAddHeader h s = lift (cgiAddHeader h s)
  cgiGet f         = lift (cgiGet f)

instance MonadCGI m => MonadCGI (StateT r m) where
  cgiAddHeader h s = lift (cgiAddHeader h s)
  cgiGet f         = lift (cgiGet f)

instance MTL.MonadIO m => MTL.MonadIO (ReaderT r m) where
  liftIO = lift . MTL.liftIO

instance MTL.MonadIO m => MTL.MonadIO (StateT r m) where
  liftIO = lift . MTL.liftIO
