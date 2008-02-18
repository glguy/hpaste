module Bot where

import Control.Concurrent
import Network.IRC

data Announcement = NewPaste Int    -- ! Paste id
                             String -- ! Author
                             String -- ! Title

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
listener handle out_chan = undefined

writer :: Chan Message -> Handle -> IO ()
writer out_chan handle = undefined

announcer :: Chan Announcement -> Chan Message -> IO ()
announce announce_chan out_chan = undefined
