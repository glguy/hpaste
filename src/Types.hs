{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Typeable

data Paste = Paste
      { paste_id        :: Int
      , paste_timestamp :: String
      , paste_content   :: String
      , paste_title     :: Maybe String
      , paste_author    :: Maybe String
      , paste_hostname  :: Maybe String
      , paste_ipaddress :: Maybe String
      , paste_expireon  :: Maybe Int
      , paste_language  :: Maybe String
      , paste_channel   :: Maybe String
      , paste_parentid  :: Maybe Int
      }
 deriving (Typeable, Show)

