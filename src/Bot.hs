module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import Network.IRC
import System.CPUTime
import System.IO
import Utils.Misc

import Bot.Base
import Types
import Storage

main :: IO ()
main =
 bracket (connect host port) hClose $ \ handle ->
 do done <- newEmptyMVar
    out_chan <- newChan
    runM handle done out_chan $
     do initialize botnick username realname
        fork listener
        fork writer
        fork $ announcer fifo baseurl
    takeMVar done
    clearChannels

  where
        host = "irc.freenode.org"
        baseurl = "http://localhost/cgi-bin/hpaste.fcgi/"
        botnick = "hpaste__"
        username = "hpaste"
        realname = "announcer"
        port = PortNumber 6666
        fifo = "announce.fifo"

connect :: String -> PortID -> IO Handle
connect host port =
 do h <- connectTo host port
    hSetBuffering h NoBuffering
    return h

initialize botnick username realname =
 do send_message $ nick botnick
    send_message $ user username "0" "*" realname
    send_message $ joinChan "#glguy-hpaste"

listener :: M ()
listener = forever $
 do message   <- read_message
    responses <- handle_message message
    schedule_messages responses

handle_message m =
  case m of
    Message p "PING" xs -> return [Message p "PONG" xs]
    Message _ "JOIN" [chan,_] -> io (addChannel chan) >> return []
    Message _ "PART" [chan,_] -> io (delChannel chan) >> return []
    Message _ "PRIVMSG" [_,'j':' ':chan] -> return [joinChan chan]
    Message _ "PRIVMSG" [_,'p':' ':chan] -> return [part chan]
    Message _ "PRIVMSG" [_,"quit"] -> end_bot >> return []
    _ -> io (print m) >> return []

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

announcer :: FilePath -> String -> M ()
announcer fifo baseurl =
 do s <- io $ listenOn $ UnixSocket "pastes/announce"
    (forever $ do (h,_,_) <- io $ accept s
                  xs <- io $ hGetLine h
                  io $ print xs
                  announce baseurl (read xs)
     )

announce baseurl a =
 do res <- io $ getPaste a
    whenJust res $ \ paste -> do
      let c = paste_channel paste
      chans <- io $ getChannels
      when (c `elem` chans) $
       schedule_messages [ privmsg c $ "New paste: " ++ paste_title paste ]
