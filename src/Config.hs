module Config where

import Utils.Misc

data Config = Config
  { db_path     :: FilePath
  , style_path  :: String
  , pastes_per_page :: Int
  , default_language :: String
  } deriving (Read,Show)

default_config :: Config
default_config = Config
  { db_path     = "hpaste/hpaste.db"
  , style_path  = "/"
  , pastes_per_page = 20
  , default_language = "Haskell"
  }

getConfig :: IO Config
getConfig =
  do txt <- readFile "hpaste.conf"
     case maybeRead txt of
       Just conf -> return conf
       Nothing -> return default_config
   `catch` \_ -> return default_config
