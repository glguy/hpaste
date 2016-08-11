{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Typeable
import Data.Time

data Paste = Paste
      { paste_id        :: Int
      , paste_timestamp :: !(Maybe UTCTime)
      , paste_content   :: String
      , paste_title     :: String
      , paste_author    :: String
      -- ..md5 sum of email address..
      , paste_hostname  :: Maybe String
      , paste_ipaddress :: String
      , paste_expireon  :: Maybe Int
      , paste_language  :: String
      , paste_channel   :: String
      , paste_parentid  :: Maybe Int
      }
 deriving (Typeable, Show)

data User = User
      { user_id         :: Int
      , user_name       :: String
      , user_password   :: String
      , user_ircmask    :: Maybe String
      , user_admin      :: Bool
      }
  deriving (Typeable, Show)

type SessionId = Int
