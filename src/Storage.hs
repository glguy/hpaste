{-# LANGUAGE DeriveDataTypeable #-}
module Storage where

import Database.Sqlite.Enumerator
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

dbConnect = connect "pastes/pastes.db"

getPastes =
 withSession dbConnect $
   let query = sql "select pasteid, title from paste order by createstamp DESC"

       iter :: Monad m => Int -> String -> IterAct m [(Int, String)]
       iter a b acc = result' ( (a,b) : acc )

   in reverse `fmap` doQuery query iter []

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
             -> Maybe String
             -> Maybe String
             -> Maybe String
             -> Maybe String
             -> Maybe Int
             -> Maybe String
             -> Maybe String
             -> Maybe Int
             -> IterAct m (Maybe Paste)
iterFirst a b c d e f g h i j k Nothing =
         return $ Left $ Just $ Paste a b c d e f g h i j k

writePaste :: String -> String -> String -> IO (Either String Int)
writePaste title author content =
  let query1 = sql "insert into paste (title, author, content) values (?,?,?)"
      query2 = sql "select last_insert_rowid()"

      iterFirst :: Monad m => Int -> IterAct m (Either String Int)
      iterFirst i _ = return $ Left $ Right i

      bindings = [bindP title, bindP author, bindP content]
  in (
  withSession dbConnect $
  withPreparedStatement (prepareQuery query1) $ \ pstmt ->
  withBoundStatement pstmt bindings           $ \ bstmt ->
   do execDML bstmt
      (Right . fromIntegral) `fmap` inquire LastInsertRowid
  ) `catchDB` \ e -> return (Left (show e))
