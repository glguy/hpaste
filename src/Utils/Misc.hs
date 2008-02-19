module Utils.Misc where

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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

drop_prefix :: String -> String -> Maybe String
drop_prefix [] xs = Just xs
drop_prefix (x:xs) (y:ys) | x == y = drop_prefix xs ys
drop_prefix _ _ = Nothing
