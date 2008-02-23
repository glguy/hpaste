module Storage where

import Database.Sqlite.Enumerator
import Types

dbConnect = connect "pastes/pastes.db"

getPastes :: Int -> Int -> IO [Paste]
getPastes limit offset =
  withSession dbConnect $
  withPreparedStatement (prepareQuery query) $ \ pstmt ->
  withBoundStatement pstmt bindings $ \ bstmt ->
  reverse `fmap` doQuery bstmt iterAll []
  where
  query = sql "SELECT * from paste order by createstamp DESC LIMIT ? OFFSET ?"
  bindings = [bindP limit, bindP offset]

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
             -> Maybe String
             -> Maybe Int
             -> String
             -> String
             -> Maybe Int
             -> IterAct m (Maybe Paste)
iterFirst a b c d e f g h i j k Nothing =
         return $ Left $ Just $ Paste a b c d e f g h i j k

iterAll      :: (Monad m) =>
             Int
             -> String
             -> String
             -> String
             -> String
             -> Maybe String
             -> Maybe String
             -> Maybe Int
             -> String
             -> String
             -> Maybe Int
             -> IterAct m [Paste]
iterAll a b c d e f g h i j k xs =
         result' $ Paste a b c d e f g h i j k : xs

writePaste :: Paste -> IO (Either String Int)
writePaste p =
  let query1 = sql "insert into paste (title, author, content, language, channel, parentid) values (?,?,?,?,?,?)"
      query2 = sql "select last_insert_rowid()"

      iterFirst :: Monad m => Int -> IterAct m (Either String Int)
      iterFirst i _ = return $ Left $ Right i

      bindings = [bindP (paste_title p), bindP (paste_author p),
                  bindP (paste_content p), bindP (paste_language p),
                  bindP (paste_channel p), bindP (paste_parentid p)]
  in (
  withSession dbConnect $
  withPreparedStatement (prepareQuery query1) $ \ pstmt ->
  withBoundStatement pstmt bindings           $ \ bstmt ->
   do execDML bstmt
      (Right . fromIntegral) `fmap` inquire LastInsertRowid
  ) `catchDB` \ e -> return (Left (show e))
