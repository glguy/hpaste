module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.Typeable
import Data.Maybe
import Network
import Network.IRC
import System.CPUTime
import System.IO
import Utils.Misc

import Bot.Base
import Types
import Storage
import Config
import Utils.URL
import API

main :: IO ()
main = do
 conf <- getConfig
 bracket (connect (irc_host conf) (irc_port conf)) hClose $ \ handle -> do
    done <- newEmptyMVar
    out_chan <- newChan
    runM conf handle done out_chan $
     do initialize
        exec_db $ clearChannels
        fork listener
        fork writer
        fork announcer
        io $ takeMVar done
        exec_db $ clearChannels
        return ()

connect :: String -> Int -> IO Handle
connect host port =
 do h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return h

initialize =
 do conf <- current_config
    send_message $ nick (irc_nick conf)
    send_message $ user (irc_username conf) "0" "*" (irc_realname conf)

listener :: M ()
listener = forever $
 do message   <- read_message
    responses <- handle_message message `catchall` return []
    schedule_messages responses

handle_message m =
  case m of
    Message p "PING" xs -> return [Message p "PONG" xs]
    Message _ "PRIVMSG" _ -> handle_privmsg m
    _ -> io (print m) >> return []

handle_privmsg m@(Message (Just (NickName n _ (Just mask))) _ [target,what]) =
 do conf <- current_config
    if target == irc_nick conf then do
      user <- exec_db $ getUserByMask mask
      case user of
       Just u | user_admin u && user_name u == n ->
        case what of
             'j':' ':chan -> do exec_db $ addChannel chan
                                return [joinChan chan]
             'p':' ':chan -> do exec_db $ delChannel chan
                                return [part chan]
             "quit"       -> end_bot >> return []
             _            -> io (putStrLn "no command" >> print m) >> return []
       _ -> io (print m) >> return []
      else if (irc_nick conf ++ ": url") `isPrefixOf` what then
        return [privmsg target (base_url conf ++
                                 exportURL (methodURL mList Nothing Nothing))]
      else return []

handle_privmsg m = io (print m) >> return []

writer :: M ()
writer =
 do foldM waiter 0 =<< list_scheduled_messages
    return ()

waiter :: Integer -> Message -> M Integer
waiter flood_time message =
 do now <- io $ fmap (`div` cpuTimePrecision) getCPUTime
    when (flood_time - now > 7) $ io $ threadDelay 2000000
    send_message message
    return $ max (now + 2) (flood_time + 2)

announcer :: M ()
announcer =
 do conf <- current_config
    s <- io $ listenOn $ UnixSocket $ announce_socket conf
    (forever $ do (h,_,_) <- io $ accept s
                  xs <- io $ hGetLine h
                  io $ print xs
                  announce (base_url conf) (read xs)
     )

announce baseurl a =
 do res <- exec_db $ getPaste a
    whenJust res $ \ paste -> do
      parentid <- exec_db $ topmost_parent $ paste_parentid paste
      let c = paste_channel paste
      chans <- exec_db $ getChannels
      when (c `elem` chans) $
       schedule_messages [privmsg c $ paste_to_announce baseurl paste
                                        $ fromMaybe (paste_id paste) parentid]

paste_to_announce baseurl paste topmost_id =
  " \"" ++ paste_author paste ++ "\" pasted \""
  ++ paste_title paste ++ "\" at "
  ++ baseurl ++ exportURL (methodURL mView topmost_id)

exec_db :: Typeable a => StoreM a -> M a
exec_db m = do conf <- current_config
               io $ runStoreM (db_path conf) m
