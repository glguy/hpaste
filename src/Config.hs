{-# Language ScopedTypeVariables #-}
module Config where

import Control.Exception
import Utils.Misc

-- | The configuration type for hpaste.
data Config = Config
  { db_path             :: FilePath -- ^ path to database
  , style_path          :: [String]
  , pastes_per_page     :: !Int
  , default_language    :: String
  , base_url            :: String   -- ^ base url of service
  , rss_path            :: FilePath -- ^ rss file to generate to
  , announce_socket     :: String
  , max_paste_length    :: !Int
  } deriving (Read,Show)

default_config :: Config
default_config = Config
  { db_path             = "hpaste/hpaste.db"
  , style_path          = []
  , pastes_per_page     = 20
  , default_language    = "Haskell"
  , base_url            = "http://localhost/cgi-bin/hpaste.fcgi/"
  , rss_path            = "recent.rss"
  , announce_socket     = "announce"
  , max_paste_length    = 50000
  }

loadConfig :: IO Config
loadConfig =
  do txt <- readFile "hpaste.conf"
     case maybeRead txt of
       Just conf -> return conf
       Nothing  -> fail "Failed to parse config file!"
     `catch` \(_::IOError) -> return default_config
