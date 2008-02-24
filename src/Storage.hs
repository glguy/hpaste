module Storage where

import Database.Sqlite.Enumerator
import Types
import Data.Time
import Data.Time.Format
import System.Locale


parse_time :: String -> Maybe UTCTime
parse_time = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

dbConnect = connect "pastes/pastes.db"

getPastes :: Maybe String -> Int -> Int -> IO [Paste]
getPastes mpat limit offset =
  withSession dbConnect $
  withPreparedStatement (prepareQuery query) $ \ pstmt ->
  withBoundStatement pstmt bindings $ \ bstmt ->
  reverse `fmap` doQuery bstmt iterAll []
  where
  query = sql $ "SELECT * FROM paste" ++ cond ++ " ORDER BY createstamp" ++
                " DESC LIMIT ? OFFSET ?"
  (param,cond) = case mpat of
                   Just pat -> ([bindP pat]," WHERE content LIKE ?")
                   Nothing -> ([], "")
  bindings = param ++ [bindP limit, bindP offset]

getChildren :: Int -> IO [Paste]
getChildren parentid =
  withSession dbConnect $
  withPreparedStatement (prepareQuery query) $ \ pstmt ->
  withBoundStatement pstmt [bindP parentid] $ \ bstmt ->
  reverse `fmap` doQuery bstmt iterAll []
  where
  query = sql $ "SELECT * from paste WHERE parentid = ? "
             ++ "ORDER BY createstamp ASC"

getPaste :: Int -> IO (Maybe Paste)
getPaste pasteId =
    withSession dbConnect                      $
    withPreparedStatement (prepareQuery query) $ \ pstmt ->
    withBoundStatement pstmt [bindP pasteId]   $ \ bstmt ->
    doQuery bstmt iterFirst (Nothing :: Maybe Paste)

 where query = sql $ "SELECT * FROM paste WHERE pasteid = ?"


iterFirst :: (Monad m) =>
             Int
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
             -> IterAct m (Maybe Paste)
iterFirst a b c d e f g h i j k Nothing =
         return $ Left $ Just $ Paste a (parse_time b) c d e f g h i j k

iterAll      :: (Monad m) =>
             Int
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
             -> IterAct m [Paste]
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
  in (
  withSession dbConnect $
  withPreparedStatement (prepareQuery query1) $ \ pstmt ->
  withBoundStatement pstmt bindings           $ \ bstmt ->
   do execDML bstmt
      (Right . fromIntegral) `fmap` inquire LastInsertRowid
  ) `catchDB` \ e -> return (Left (show e))

getChannels :: IO [String]
getChannels = withSession dbConnect $ reverse `fmap` doQuery query iter []
  where
  query = sql "SELECT channelname from channel ORDER BY channelname"
  iter :: Monad m => String -> IterAct m [String]
  iter x xs = result' (x:xs)

addChannel :: String -> IO ()
addChannel chan = (
  withSession dbConnect $
  withPreparedStatement (prepareQuery query)  $ \ pstmt ->
  withBoundStatement pstmt [bindP chan]       $ \ bstmt ->
  execDML bstmt >>
  return ()
  ) `catchDB` \ _ -> return ()
  where
  query = sql "INSERT INTO channel (channelname) VALUES (?)"
