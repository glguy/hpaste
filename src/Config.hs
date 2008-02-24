module Config where

data Config = Config
  { db_path     :: FilePath
  , style_path  :: String
  } deriving (Read,Show)

default_config :: Config
default_config = Config
  { db_path     = "hpaste/hpaste.db"
  , style_path  = "/"
  }


