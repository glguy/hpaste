{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Database.Sqlite.Enumerator
import Codec.Binary.UTF8.String as UTF8
import Data.Maybe (listToMaybe,fromMaybe)
import Data.Typeable

newtype StoreM a = SM (ReaderT FilePath IO a)
  deriving (Functor,Monad)

instance BaseM StoreM IO where
  inBase = SM . lift . inBase

get_db :: StoreM FilePath
get_db = SM ask

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = runReaderT db m

data Handle bstmt s a = Handler (forall mark. bstmt -> DBM mark s a)
data QueryH st s a = QueryH (forall m. PreparedStmt m st -> DBM m s a)

with_query query (QueryH f) =
 do db <- get_db
    inBase $ withSession (connect db)
      (withPreparedStatement (prepareQuery (sql query)) f)
      `catchDB` \ e -> fail (formatDBException e)

with_db query bindings (Handler f) =
  with_query query (QueryH (\ pstmt ->
  withBoundStatement pstmt bindings f))

many_with_db query bindingss (Handler f) =
 do db <- get_db
    inBase $ (withSession (connect db)
             (forM_ bindingss (\ bindings ->
             withPreparedStatement stmt (\ pstmt ->
             withBoundStatement pstmt bindings f))))
      `catchDB` \ e -> fail (formatDBException e)
  where stmt = prepareQuery (sql query)

exec a b      = with_db a b dmlHandler
execMany a bs = many_with_db a bs dmlHandler
dmlHandler    = Handler (\ bstmt -> execDML bstmt >> return ())

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset =
  with_db query (param ++ [bindP limit, bindP offset])
                allPastes
  where
  query = "SELECT * FROM paste" ++ cond ++
          " ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  (param,cond) = case mpat of
                   Just pat -> ([bindP pat]," WHERE content LIKE ?")
                   Nothing -> ([], " WHERE parentid IS NULL")

getChildren :: Int -> StoreM [Paste]
getChildren parentid =
  with_db query [bindP parentid] allPastes where
  query = "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = with_db query [bindP pasteId] onePaste
  where query = "SELECT * FROM paste WHERE pasteid = ?"

getAnnotations :: Int -> StoreM [Int]
getAnnotations pasteId = with_db query [bindP pasteId] allAnnotations
  where query = "SELECT line FROM annotation WHERE pasteid = ?"

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] =
  exec "DELETE FROM annotation WHERE pasteid = ?" [bindP pid]

delAnnotations pid ls = execMany query binds
  where
  query = "DELETE FROM annotation WHERE pasteid = ? AND line = ?"
  binds = [ [bindP pid,bindP l] | l <- ls ]


addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls = execMany query binds
  where
  query = "REPLACE INTO annotation (pasteid,line) VALUES (?,?)"
  binds = [ [bindP pid, bindP l] | l <- ls ]



writePaste :: Paste -> StoreM Int
writePaste p = with_db query bindings lastRowHandler
  where
  query = "INSERT INTO paste (title, author, content, language, " ++
          "channel, parentid, ipaddress, hostname)" ++
          " VALUES (?,?,?,?,?,?,?,?)"
  bindings =  [ bindP (paste_title p), bindP (paste_author p)
              , bindP (paste_content p)
              , bindP (paste_language p)
              , bindP (paste_channel p), bindP (paste_parentid p)
              , bindP (paste_ipaddress p), bindP (paste_hostname p)
              ]

getChannels :: StoreM [String]
getChannels =
  with_db "SELECT channelname from channel ORDER BY channelname" [] allChannels


addChannel :: String -> StoreM ()
addChannel chan =
  exec "INSERT INTO channel (channelname) VALUES (?)" [bindP chan]

delChannel :: String -> StoreM ()
delChannel chan =
  exec "DELETE FROM channel WHERE channelname = ?" [bindP chan]

clearChannels :: StoreM ()
clearChannels = exec "DELETE FROM channel" []

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ fromMaybe (paste_id ppaste) (paste_parentid ppaste)
  where bind m f = maybe (return Nothing) f =<< m


allAnnotations = Handler (\ bstmt -> reverse `fmap` doQuery bstmt iter [])
  where
  iter :: Monad m => Int -> IterAct m [Int]
  iter x acc = result' (x : acc)

type PasteIter x
  = Int -> String -> String -> String -> String -> Maybe String -> String
  -> Maybe Int -> String -> String -> Maybe Int -> x

allPastes = Handler (\ bstmt -> reverse `fmap` doQuery bstmt iter [])
  where
  iter :: Monad m => PasteIter (IterAct m [Paste])
  iter a b c d e f g h i j k acc =
    result' (Paste a (parse_time b) c d e f g h i j (zeroIsNothing k) : acc)

onePaste = Handler (\ bstmt -> doQuery bstmt iter Nothing)
  where
  iter :: Monad m => PasteIter (IterAct m (Maybe Paste))
  iter a b c d e f g h i j k _ =
    return $ Left $ Just $ Paste a (parse_time b) c d e f g h i j (zeroIsNothing k)

zeroIsNothing (Just 0) = Nothing
zeroIsNothing x = x

lastRowHandler = Handler (\ bstmt ->
   execDML bstmt >> fromIntegral `fmap` inquire LastInsertRowid)

allChannels = Handler (\ bstmt -> doQuery bstmt iter [])
  where
  iter :: Monad m => String -> IterAct m [String]
  iter a acc = result' (a : acc)
