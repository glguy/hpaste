module Config where

import Utils.Misc

-- | The configuration type for hpaste.
data Config = Config
  { db_path             :: FilePath
  , style_path          :: [String]
  , pastes_per_page     :: !Int
  , default_language    :: String
  , base_url            :: String
  , rss_path            :: FilePath
  , irc_host            :: String
  , irc_nick            :: String
  , irc_username        :: String
  , irc_realname        :: String
  , irc_port            :: !Int
  , announce_socket     :: String
  } deriving (Read,Show)

default_config :: Config
default_config = Config
  { db_path             = "hpaste/hpaste.db"
  , style_path          = []
  , pastes_per_page     = 20
  , default_language    = "Haskell"
  , base_url            = "http://localhost/cgi-bin/hpaste.fcgi/"
  , rss_path            = "recent.rss"
  , irc_host            = "irc.freenode.org"
  , irc_nick            = "hpaste__"
  , irc_username        = "hpaste"
  , irc_realname        = "announcer"
  , irc_port            = 6666
  , announce_socket     = "announce"
  }

getConfig :: IO Config
getConfig =
  do txt <- readFile "hpaste.conf"
     case maybeRead txt of
       Just conf -> return conf
       Nothing  -> fail "Failed to parse config file!"
     `catch` \_ -> return default_config

