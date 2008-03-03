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

import MonadLib
import Database.HDBC.Sqlite3
import Database.HDBC
import Codec.Binary.UTF8.String as UTF8

sqlToString = UTF8.decodeString . fromSql
stringToSql = toSql . UTF8.encodeString

newtype StoreM a = SM (ReaderT FilePath IO a) deriving (Functor,Monad)

get_db :: StoreM FilePath
get_db = SM ask

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = runReaderT db m


with_db :: (Connection -> IO a) -> StoreM a
with_db f =
 get_db >>= \ db -> SM $ lift $ handleSqlError $ do
    c <- connectSqlite3 db
    x <- f c
    disconnect c
    return x

select query params parse = with_db $ \ c ->
    fmap parse `fmap` quickQuery' c query params

select1 query params parse = with_db $ \ c ->
 do stmt <- prepare c query
    execute stmt params
    r <- fmap parse `fmap` fetchRow stmt
    finish stmt
    return r

execMany query paramss = with_db $ \ c ->
 do stmt <- prepare c query
    executeMany stmt paramss
    commit c

exec a b = with_db $ \ db -> run db a b >> commit db

last_row_id :: IConnection c => c -> IO Int
last_row_id db =
  do stmt <- prepare db "select last_insert_rowid()"
     execute stmt []
     Just [i] <- fetchRow stmt
     return (fromSql i)


getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset =
  select query (param ++ [toSql limit, toSql offset]) toPaste
  where
  query = "SELECT * FROM paste" ++ cond ++
          " ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  (param,cond) = case mpat of
                   Just pat -> ([toSql pat]," WHERE content LIKE ?")
                   Nothing -> ([], " WHERE parentid IS NULL")

getChildren :: Int -> StoreM [Paste]
getChildren parentid = select query [toSql parentid] toPaste where
  query = "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = select1 query [toSql pasteId] toPaste
  where query = "SELECT * FROM paste WHERE pasteid = ?" 

getAnnotations :: Int -> StoreM [Int]
getAnnotations pasteId = select query [toSql pasteId] toAnnotation
  where query = "SELECT line FROM annotation WHERE pasteid = ?"

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] =
  exec "DELETE FROM annotation WHERE pasteid = ?" [toSql pid]

delAnnotations pid ls = execMany query binds
  where
  query = "DELETE FROM annotation WHERE pasteid = ? AND line = ?"
  binds = [ [toSql pid,toSql l] | l <- ls ]


addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls = execMany query binds
  where
  query = "REPLACE INTO annotation (pasteid,line) VALUES (?,?)"
  binds = [ [toSql pid, toSql l] | l <- ls ]



writePaste :: Paste -> StoreM Int
writePaste p = with_db $ \ db ->
 do let query = "INSERT INTO paste (title, author, content, language, " ++
                "channel, parentid, ipaddress, hostname)" ++
                " VALUES (?,?,?,?,?,?,?,?)"
        bindings =  [ stringToSql (paste_title p), stringToSql (paste_author p)
                    , stringToSql (paste_content p)
                    , stringToSql (paste_language p)
                    , stringToSql (paste_channel p), toSql (paste_parentid p)
                    , toSql (paste_ipaddress p), toSql (paste_hostname p)
                    ]
    run db query bindings
    i <- last_row_id db
    commit db
    return i

getChannels :: StoreM [String]
getChannels =
  select "SELECT channelname from channel ORDER BY channelname" [] toChannel


addChannel :: String -> StoreM ()
addChannel chan =
  exec "INSERT INTO channel (channelname) VALUES (?)" [toSql chan]

delChannel :: String -> StoreM ()
delChannel chan =
  exec "DELETE FROM channel WHERE channelname = ?" [toSql chan]

clearChannels :: StoreM ()
clearChannels = exec "DELETE FROM channel"  []

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ case paste_parentid ppaste of
                    Just i | i > 0 -> i
                    _ -> paste_id ppaste

  where bind m f = maybe (return Nothing) f =<< m

toAnnotation :: [SqlValue] -> Int
toAnnotation [a] = fromSql a
toAnnotation _ = error "toAnnotation: bad list length"

toPaste :: [SqlValue] -> Paste
toPaste [a,b,c,d,e,f,g,h,i,j,k] =
  Paste (fromSql a) (parse_time $ fromSql b) (sqlToString c) (sqlToString d)
        (fromSql e) (fromSql f) (fromSql g) (fromSql h) (sqlToString i)
        (sqlToString j)(fromSql k)
toPaste _ = error "toPaste: list length wrong"

toChannel :: [SqlValue] -> String
toChannel [a] = sqlToString a
toChannel _ = error "toChannel: list wrong length"
