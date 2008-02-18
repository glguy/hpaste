module Utils.Misc

import qualified Data.ByteString as BS
import qualified Codec.Binary.UTF8.String as UTF8

maybeRead :: Read a => String -> Maybe a
maybeRead xs = case reads xs of
                [(y,"")] -> Just y
                _        -> Nothing


utf8_decode :: BS.ByteString -> String
utf8_decode = UTF8.decode . BS.unpack

utf8_encode :: String -> BS.ByteString
utf8_encode = BS.pack . UTF8.encode

