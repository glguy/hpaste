module Utils.Misc where

import Data.Time
import Data.Time.Format
import System.Locale
import qualified Data.ByteString as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char (isSpace)

maybeRead :: Read a => String -> Maybe a
maybeRead xs = case reads xs of
                [(y,xs)] | all isSpace xs -> Just y
                _        -> Nothing


utf8_decode :: BS.ByteString -> String
utf8_decode = UTF8.decode . BS.unpack

utf8_encode :: String -> BS.ByteString
utf8_encode = BS.pack . UTF8.encode

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

drop_prefix :: String -> String -> Maybe String
drop_prefix [] xs = Just xs
drop_prefix (x:xs) (y:ys) | x == y = drop_prefix xs ys
drop_prefix _ _ = Nothing

parse_time :: String -> Maybe UTCTime
parse_time = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

