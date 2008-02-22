{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Typeable

data Paste = Paste
      { paste_id        :: Int
      , paste_timestamp :: String
      , paste_content   :: String
      , paste_title     :: String
      , paste_author    :: String
      , paste_hostname  :: Maybe String
      , paste_ipaddress :: Maybe String
      , paste_expireon  :: Maybe Int
      , paste_language  :: String
      , paste_channel   :: String
      , paste_parentid  :: Maybe Int
      }
 deriving (Typeable, Show)

