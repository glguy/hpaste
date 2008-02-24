{-# LANGUAGE Rank2Types #-}
module Storage where

import Database.Sqlite.Enumerator
import Types
import Data.Time
import Data.Time.Format
import System.Locale


parse_time :: String -> Maybe UTCTime
parse_time = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

dbConnect = connect "pastes/pastes.db"

data Handler bstmt s a = Handler (forall mark. bstmt -> DBM mark s a)

with_db query bindings (Handler f) =
  withSession dbConnect (
  withPreparedStatement (prepareQuery query) (\ pstmt ->
  withBoundStatement pstmt bindings f
  ))

getPastes :: Maybe String -> Int -> Int -> IO [Paste]
getPastes mpat limit offset =
  with_db query bindings $ Handler (\bstmt ->
    reverse `fmap` doQuery bstmt iterAll []
  )
  where
  query = sql $ "SELECT * FROM paste" ++ cond ++ " ORDER BY createstamp" ++
                " DESC LIMIT ? OFFSET ?"
  (param,cond) = case mpat of
                   Just pat -> ([bindP pat]," WHERE content LIKE ?")
                   Nothing -> ([], "")
  bindings = param ++ [bindP limit, bindP offset]

getChildren :: Int -> IO [Paste]
getChildren parentid = with_db query [bindP parentid] $ Handler (\bstmt ->
    reverse `fmap` doQuery bstmt iterAll []
  )
  where
  query = sql $ "SELECT * from paste WHERE parentid = ? "
             ++ "ORDER BY createstamp ASC"

getPaste :: Int -> IO (Maybe Paste)
getPaste pasteId = with_db query [bindP pasteId] $ Handler (\bstmt ->
    doQuery bstmt iterFirst (Nothing :: Maybe Paste)
  )

 where query = sql $ "SELECT * FROM paste WHERE pasteid = ?"


-- | Should correspond to the fields in the table @paste@.
type PasteIter x
  = Int
 -> String
 -> String
 -> String
 -> String
 -> Maybe String
 -> String
 -> Maybe Int
 -> String
 -> String
 -> Maybe Int
 -> x

iterFirst :: (Monad m) => PasteIter (IterAct m (Maybe Paste))
iterFirst a b c d e f g h i j k Nothing =
         return $ Left $ Just $ Paste a (parse_time b) c d e f g h i j k

iterAll   :: (Monad m) => PasteIter (IterAct m [Paste])
iterAll a b c d e f g h i j k xs =
         result' $ Paste a (parse_time b) c d e f g h i j k : xs

writePaste :: Paste -> IO (Either String Int)
writePaste p =
  let query1 = sql "insert into paste (title, author, content, language, channel, parentid, ipaddress, hostname) values (?,?,?,?,?,?,?,?)"
      query2 = sql "select last_insert_rowid()"

      iterFirst :: Monad m => Int -> IterAct m (Either String Int)
      iterFirst i _ = return $ Left $ Right i

      bindings = [bindP (paste_title p), bindP (paste_author p),
                  bindP (paste_content p), bindP (paste_language p),
                  bindP (paste_channel p), bindP (paste_parentid p),
                  bindP (paste_ipaddress p), bindP (paste_hostname p)]
  in with_db query1 bindings $ Handler (\bstmt ->
   do execDML bstmt
      (Right . fromIntegral) `fmap` inquire LastInsertRowid
    `catchDB` \ e -> return (Left (show e))
   )

getChannels :: IO [String]
getChannels = with_db query [] $ Handler (\bstmt ->
    reverse `fmap` doQuery bstmt iter []
  )
  where
  query = sql "SELECT channelname from channel ORDER BY channelname"
  iter :: Monad m => String -> IterAct m [String]
  iter x xs = result' (x:xs)

addChannel :: String -> IO ()
addChannel chan = with_db query [bindP chan] $ Handler (\bstmt ->
    do execDML bstmt
       return ()
     `catchDB` \ _ -> return ()
  )
  where
  query = sql "INSERT INTO channel (channelname) VALUES (?)"
