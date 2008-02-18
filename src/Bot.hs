module Bot where

import Control.Concurrent
import Network.IRC

data Announcement = NewPaste { pasteid :: !Int        -- ! Paste id
                             , author  :: String     -- ! Author
                             , title   :: String }   -- ! Title

spawn_bot :: IO (Chan Announcement)
spawn_bot =
 do handle <- connect "server"
    out_chan <- newChan
    announce_chan <- newChan
    forkIO $ listener handle out_chan
    forkIO $ writer out_chan handle
    forkIO $ announcer announce_chan out_chan
    return announce_chan

connect :: String -> IO Handle
connect host = undefined

listener :: Handle -> Chan Message -> IO ()
listener handle out_chan = forever $
 do line <- hGetLine handle
    whenJust (decode line) $ \ message ->
     do response <- handle_message message
        writeChan out_chan response

writer :: Chan Message -> Handle -> IO ()
writer out_chan handle =
 do out_list <- getChanContents out_chan
    foldM (waiter handle) 0 out_list

waiter :: Integer -> Message -> IO Integer
waiter handle flood_time message =
 do now <- fmap (`div` cpuTimePrecision) getCPUTime
    when (flood_time - now > 7) $ threadDelay 2000000
    hPutStrLn handle $ showMessage message
    return $ max (now + 2) (flood_time + 2)

announcer :: Chan Announcement -> Chan Message -> IO ()
announce announce_chan out_chan = undefined
