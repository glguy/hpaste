module Bot where

import Control.Concurrent
import Network.IRC
import Network
import Utils.Misc
import System.IO
import System.CPUTime
import Control.Monad

data Announcement = NewPaste { pasteid :: !Int        -- ! Paste id
                             , announce_channel :: String
                             , author  :: String     -- ! Author
                             , title   :: String }   -- ! Title

main =
 do announce_chan <- spawn_bot "irc.freenode.org" (PortNumber 6666)
    forever $ do
      line <- getLine
      writeChan announce_chan $ NewPaste 1 "#glguy-hpaste" "glguy" line

spawn_bot :: String -> PortID -> IO (Chan Announcement)
spawn_bot host port =
 do handle <- connect host port
    out_chan <- newChan
    announce_chan <- newChan
    forkIO $ listener handle out_chan
    forkIO $ writer out_chan handle
    forkIO $ announcer announce_chan out_chan
    return announce_chan

connect :: String -> PortID -> IO Handle
connect host port =
 do h <- connectTo host port
    hSetBuffering h NoBuffering
    hPutStrLn h $ "NICK hpaste__"
    hPutStrLn h $ "USER hpaste__ 0 * :announcer"
    hPutStrLn h $ "JOIN #glguy-hpaste"
    return h

listener :: Handle -> Chan Message -> IO ()
listener handle out_chan = forever $
 do line <- hGetLine handle
    case decode (line ++ "\n") of
      Nothing -> print line
      Just message ->
       do responses <- handle_message message
          writeList2Chan out_chan responses

handle_message m@(Message p "PING" xs) = print m >> return [Message p "PONG" xs]
handle_message m@(Message _ "PRIVMSG" _) = print m >> return [Message Nothing "NOTICE" ["#glguy-hpaste",show m]]
handle_message m = print m >> return []

writer :: Chan Message -> Handle -> IO ()
writer out_chan handle =
 do out_list <- getChanContents out_chan
    foldM (waiter handle) 0 out_list
    return ()

waiter :: Handle -> Integer -> Message -> IO Integer
waiter handle flood_time message =
 do now <- fmap (`div` cpuTimePrecision) getCPUTime
    when (flood_time - now > 7) $ threadDelay 2000000
    hPutStrLn handle $ showMessage message
    return $ max (now + 2) (flood_time + 2)

announcer :: Chan Announcement -> Chan Message -> IO ()
announcer announce_chan out_chan =
 do announces <- getChanContents announce_chan
    mapM_ (announce out_chan) announces

announce out_chan a = writeChan out_chan
                    $ privmsg (announce_channel a)
                    $ "New paste: " ++ show (pasteid a)
