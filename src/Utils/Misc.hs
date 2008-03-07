module Utils.Misc where

import Data.Time
import Data.Time.Format
import System.Locale
import Data.Char (isSpace)

maybeRead :: Read a => String -> Maybe a
maybeRead xs = case reads xs of
                [(y,xs)] | all isSpace xs -> Just y
                _        -> Nothing

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust _ _ = return ()

drop_prefix                             :: String -> String -> Maybe String
drop_prefix []     ys                   = Just ys
drop_prefix (x:xs) (y:ys) | x == y      = drop_prefix xs ys
drop_prefix _      _                    = Nothing

parse_time :: String -> Maybe UTCTime
parse_time = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

show_rfc1123 :: UTCTime -> String
show_rfc1123 = formatTime defaultTimeLocale rfc1123_format

read_rfc1123 :: String -> Maybe UTCTime
read_rfc1123 = parseTime defaultTimeLocale rfc1123_format

rfc1123_format :: String
rfc1123_format = "%a, %d %b %Y %H:%M:%S GMT"

rfc822_named_format_str :: String
rfc822_named_format_str  = "%a, %d %b %Y %H:%M:%S %Z"
