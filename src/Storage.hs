{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Storage
  (
  -- * The Store monad
    StoreM
  , runStoreM

  -- * Paste manipulation
  , getPaste
  , getPastes
  , getChildren
  , writePaste
  , topmost_parent

  -- * Annotations
  , getAnnotations
  , addAnnotations
  , delAnnotations

  -- * Channel manipulation
  , getChannels
  , addChannel
  , delChannel
  , clearChannels
  ) where

import Types
import Utils.Misc(parse_time)
import Control.Monad.Trans (liftIO)

import MonadLib

import Database.HDBC.Sqlite3
import Database.HDBC

newtype StoreM a = SM (ReaderT FilePath IO a) deriving (Functor,Monad)

get_db :: StoreM FilePath
get_db = SM ask

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = runReaderT db m

{-
with_statement query f =
  get_db >>= \db -> SM $ lift $ do
    c <- connectSqlite3 db
    res <- f =<< prepare c query
    disconnect c
    return res

with_db (query,bindings,f) =
  with_statement query $
    StmtH (\stmt -> withBoundStatement pstmt bindings f)


try_exec_dml s = do liftIO $ print ".."
                    n <- execDML s `catchDB` \e -> liftIO (print e) >> return 18
                    liftIO $ print n
                    n `seq` return ()
                 `catchDB` \_ -> return ()
-}

with_db :: (Connection -> IO a) -> StoreM a
with_db f =
 get_db >>= \ db -> SM $ lift $ do
    c <- connectSqlite3 db
    x <- (f c)
    disconnect c
    return x
  `catchSql` \ e -> fail (show e)

select query params parse = with_db $ \ c ->
 do stmt <- prepare c query
    execute stmt params
    fmap parse `fmap` fetchAllRows' stmt

select1 query params parse = with_db $ \ c ->
 do stmt <- prepare c query
    n <- execute stmt params
    fmap parse `fmap` fetchRow stmt

execMany query paramss = with_db $ \ c ->
 do stmt <- prepare c query
    executeMany stmt paramss
    commit c

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset =
  select ("SELECT * FROM paste" ++ cond
          ++ " ORDER BY createstamp DESC LIMIT ? OFFSET ?")
         (param ++ [toSql limit, toSql offset])
         toPaste
  where
  (param,cond) = case mpat of
                   Just pat -> ([toSql pat]," WHERE content LIKE ?")
                   Nothing -> ([], " WHERE parentid IS NULL")

getChildren :: Int -> StoreM [Paste]
getChildren parentid =
  select "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"
         [toSql parentid]
         toPaste

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId =
  select1 "SELECT * FROM paste WHERE pasteid = ?"
          [toSql pasteId]
          toPaste

getAnnotations :: Int -> StoreM [Int]
getAnnotations pasteId =
  select "SELECT line FROM annotation WHERE pasteid = ?"
         [toSql pasteId]
         toAnnotation

toAnnotation :: [SqlValue] -> Int
toAnnotation [a] = fromSql a
toAnnotation _ = error "toAnnotation: bad list length"

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] =
  run' "DELETE FROM annotation WHERE pasteid = ?" [toSql pid]

delAnnotations pid ls = execMany query binds
  where
  query = "DELETE FROM annotation WHERE pasteid = ? AND line = ?"
  binds = [ [toSql pid,toSql l] | l <- ls ]


addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls = execMany query binds
  where
  query = "REPLACE INTO annotation (pasteid,line) VALUES (?,?)"
  binds = [ [toSql pid, toSql l] | l <- ls ]


toPaste :: [SqlValue] -> Paste
toPaste [a,b,c,d,e,f,g,h,i,j,k] =
  Paste (fromSql a)(parse_time $ fromSql b)(fromSql c)(fromSql d)(fromSql e)(fromSql f)(fromSql g)(fromSql h)(fromSql i)(fromSql j)(fromSql k)
toPaste _ = error "toPaste: list length wrong"

writePaste :: Paste -> StoreM (Either String Int)
writePaste p = with_db $ \ db ->
  do stmt <- prepare db ("INSERT INTO paste "
            ++ "(title, author, content, language, channel, parentid, ipaddress, hostname)"
            ++ " VALUES (?,?,?,?,?,?,?,?)")
     execute stmt    [ toSql (paste_title p),     toSql (paste_author p)
         , toSql (paste_content p),   toSql (paste_language p)
         , toSql (paste_channel p),   toSql (paste_parentid p)
         , toSql (paste_ipaddress p), toSql (paste_hostname p)
         ]
     stmt <- prepare db "select last_insert_rowid()"
     execute stmt []
     Just [i] <- fetchRow stmt
     commit db
     return $ Right $ fromSql i

getChannels :: StoreM [String]
getChannels =
  select "SELECT channelname from channel ORDER BY channelname" [] toChannel

toChannel :: [SqlValue] -> String
toChannel [a] = fromSql a
toChannel _ = error "toChannel: list wrong length"

addChannel :: String -> StoreM ()
addChannel chan =
  run' "INSERT INTO channel (channelname) VALUES (?)" [toSql chan]

delChannel :: String -> StoreM ()
delChannel chan =
  run' "DELETE FROM channel WHERE channelname = ?" [toSql chan]

clearChannels :: StoreM ()
clearChannels = run' "DELETE FROM channel"  []

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ case paste_parentid ppaste of
                    Just i | i > 0 -> i
                    _ -> paste_id ppaste

  where bind m f = maybe (return Nothing) f =<< m

run' a b = with_db $ \ db ->
 do print a
    stmt <- prepare db a
    print =<< execute stmt b
    commit db
    return ()
