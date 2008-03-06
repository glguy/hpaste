--------------------------------------------------------------------
-- |
-- Module    : RSS
-- Copyright : (c) Don Stewart, 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module RSS where

import Types
import API
import Utils.URL
import Storage

import Text.RSS
import Network.URI
import System.Time
import System.Time.Parse
import System.Locale
import Data.Time
import System.IO

-- sure
Just logo     = parseURI "http://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Feed-icon.svg/48px-Feed-icon.svg.png"

outputRSS :: [Paste] -> String -> IO ()
outputRSS pastes url = do
   let Just homepage = parseURI url
   time <- getClockTime >>= toCalendarTime
   writeFile "recent.rss" . showXML . rssToXML $
    RSS "hpaste"
        homepage
        "hpaste: recent pastes"

        [Language "en"
        ,Copyright "(c) Don Stewart 2008"
        ,ManagingEditor "dons@galois.com (Don Stewart)"
        ,WebMaster "dons@galois.com (Don Stewart)"
        ,ChannelPubDate time
        ,LastBuildDate  time
        ,Generator "Haskell RSS"
        ,TTL (30) -- minutes
        ,Image logo
               "hpaste.org"
               homepage
               Nothing Nothing Nothing
        ]

        (map (ppr time url homepage) pastes)

rfc822_named_format_str :: String
rfc822_named_format_str  = "%a, %d %b %Y %H:%M:%S %Z"

ppr :: CalendarTime -> String -> URI -> Paste -> Item
ppr time baseurl home p =
   [ Title (paste_title p)

   , case parseURI (baseurl ++ exportURL (methodURL mView (paste_id p))) of
            Just uri -> Link uri
            Nothing  -> Link home

   , PubDate $ case paste_timestamp p of
                Nothing  -> time
                Just utc ->
                    case parseCalendarTime defaultTimeLocale rfc822_named_format_str
                            (formatTime defaultTimeLocale rfc822_named_format_str utc)
                       of Nothing -> time
                          Just t  -> t
   , Guid True $ show $
      case parseURI (baseurl ++ exportURL (methodURL mView (paste_id p))) of
            Just uri -> uri
            Nothing  -> home

   , Author (paste_author p)
   , Description (paste_content p)
   , Category Nothing (paste_language p)

   ]

