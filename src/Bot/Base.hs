{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.Base ( M()
                , catchall
                , current_config
                , end_bot
                , fork
                , io
                , list_scheduled_messages
                , read_message
                , runM
                , schedule_messages
                , send_message
                )
  where

import Config
import System.IO
import Control.Concurrent
import Control.Exception
import Network.IRC
import Prelude hiding (catch)
import MonadLib

newtype M a = M (ReaderT BotEnv IO a)
 deriving Monad

data BotEnv = BotEnv { bot_config     :: Config
                     , bot_irc_handle :: Handle
                     , bot_done_mvar  :: MVar ()
                     , bot_out_chan   :: Chan Message
                     }

runM c h d o (M m) = runReaderT (BotEnv c h d o) m

asks f = f `fmap` ask

current_config :: M Config
current_config = M $ asks bot_config

schedule_messages :: [Message] -> M ()
schedule_messages ms = M $
 do chan <- asks bot_out_chan
    inBase $ writeList2Chan chan ms

list_scheduled_messages :: M [Message]
list_scheduled_messages = M $
 do chan <- asks bot_out_chan
    inBase $ getChanContents chan

read_message :: M Message
read_message = M $
 do h <- asks bot_irc_handle
    xs <- inBase $ hGetLine h
    case decode (xs ++ "\n") of
      Nothing -> let M m = read_message in m
      Just m  -> return m

send_message :: Message -> M ()
send_message m = M $
 do h <- asks bot_irc_handle
    inBase $ hPutStrLn h $ showMessage m

end_bot :: M ()
end_bot = M $
 do mvar <- asks bot_done_mvar
    inBase $ putMVar mvar ()

io :: IO m -> M m
io m = M $ inBase m

catchall :: M a -> M a -> M a
catchall (M m) (M e) = M $
 do env <- ask
    inBase $ runReaderT env m `catch` \ _ -> runReaderT env e

fork :: M () -> M ThreadId
fork (M m) = M $
 do env <- ask
    inBase $ forkIO $ runReaderT env m
