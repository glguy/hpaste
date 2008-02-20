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

main :: IO ()
main =
 bracket (connect host port) hClose $ \ handle ->
 do done <- newEmptyMVar
    out_chan <- newChan
    runM handle done out_chan $
     do initialize botnick username realname
        fork listener
        fork writer
        fork $ announcer fifo
    takeMVar done

  where
        host = "irc.freenode.org"
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

handle_message m@(Message p "PING" xs) = return [Message p "PONG" xs]
handle_message m@(Message _ "PRIVMSG" [_,"quit"]) = end_bot >> return []
handle_message m = io (print m) >> return []

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

announcer :: FilePath -> M ()
announcer fifo = forever $
 do h <- io $ openFile fifo ReadMode
    xs <- io $ hGetLine h
    io  $ print xs
    announce xs

announce a = schedule_messages [ privmsg "#glguy-hpaste" $ "New paste: " ++ a ]
