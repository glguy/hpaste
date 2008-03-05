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

  -- * User functions
  , getUserByMask

  -- * Session functions
  , storeSessionVar
  , getSessionVar
  ) where

import Session (SessionId)
import Types
import Utils.Misc(parse_time)

import Data.Maybe (fromMaybe)
import Data.Typeable
import Database.Sqlite.Enumerator
import MonadLib

newtype StoreM a = SM (ReaderT FilePath IO a)
  deriving (Functor,Monad)

instance BaseM StoreM IO where
  inBase = SM . lift . inBase

get_db :: StoreM FilePath
get_db = SM ask

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = runReaderT db m

run_db :: Typeable a => (forall mark. DBM mark Session a) -> StoreM a
run_db m = do db <- get_db
              inBase $ withSession (connect db) m
                       `catchDB` \ e -> fail (formatDBException e)

execMany a bs = run_db (withPreparedStatement (prepareCommand (sql a)) (\ p ->
                        forM_ bs (\ b -> withBoundStatement p b execDML)))

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset = run_db (allPastes (sqlbind query bindings))
  where
  query = "SELECT * FROM paste" ++ cond ++
          " ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  bindings = param ++ [bindP limit, bindP offset]
  (param,cond) = case mpat of
                   Just pat -> ([bindP pat]," WHERE content LIKE ?")
                   Nothing -> ([], " WHERE parentid IS NULL")

getChildren :: Int -> StoreM [Paste]
getChildren parentid = run_db (allPastes (sqlbind query [bindP parentid]))
  where
  query = "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = run_db (onePaste (sqlbind query [bindP pasteId]))
  where query = "SELECT * FROM paste WHERE pasteid = ?"

getAnnotations :: Int -> StoreM [Int]
getAnnotations pasteId = run_db (allAnnotations (sqlbind query [bindP pasteId]))
  where query = "SELECT line FROM annotation WHERE pasteid = ?"

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] = run_db (execDML (cmdbind query [bindP pid]) >>
                                return ())
  where query = "DELETE FROM annotation WHERE pasteid = ?"

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
writePaste p = run_db (execDML (cmdbind query bindings) >>
                       fromIntegral `fmap` inquire LastInsertRowid)
  where
  query = "INSERT INTO paste (title, author, content, language, " ++
          "channel, parentid, ipaddress, hostname)" ++
          " VALUES (?,?,?,?,?,?,?,?)"
  bindings =  [ bindP (paste_title p)    , bindP (paste_author p)
              , bindP (paste_content p)  , bindP (paste_language p)
              , bindP (paste_channel p)  , bindP (paste_parentid p)
              , bindP (paste_ipaddress p), bindP (paste_hostname p)
              ]

getChannels :: StoreM [String]
getChannels = run_db (allChannels query)
  where query = "SELECT channelname from channel ORDER BY channelname"


addChannel :: String -> StoreM Int
addChannel chan = run_db (execDML (cmdbind query [bindP chan]))
  where query = "INSERT INTO channel (channelname) VALUES (?)"

delChannel :: String -> StoreM Int
delChannel chan = run_db (execDML (cmdbind query [bindP chan]))
  where query = "DELETE FROM channel WHERE channelname = ?"

clearChannels :: StoreM Int
clearChannels = run_db (execDML "DELETE FROM channel")

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ fromMaybe (paste_id ppaste) (paste_parentid ppaste)
  where bind m f = maybe (return Nothing) f =<< m


allAnnotations stmt = reverse `fmap` doQuery stmt iter []
  where
  iter :: Monad m => Int -> IterAct m [Int]
  iter x acc = result' (x : acc)

type PasteIter x
  = Int -> String -> String -> String -> String -> Maybe String -> String
  -> Maybe Int -> String -> String -> Maybe Int -> x

allPastes stmt = reverse `fmap` doQuery stmt iter []
  where
  iter :: Monad m => PasteIter (IterAct m [Paste])
  iter a b c d e f g h i j k acc =
    result' (Paste a (parse_time b) c d e f g h i j (zeroIsNothing k) : acc)

onePaste stmt = doQuery stmt iter Nothing
  where
  iter :: Monad m => PasteIter (IterAct m (Maybe Paste))
  iter a b c d e f g h i j k _ = oneResult $ Just $
    Paste a (parse_time b) c d e f g h i j (zeroIsNothing k)

zeroIsNothing (Just 0) = Nothing
zeroIsNothing x = x

allChannels stmt = reverse `fmap` doQuery stmt iter []
  where
  iter :: Monad m => String -> IterAct m [String]
  iter a acc = result' (a : acc)

getUserByMask :: String -> StoreM (Maybe User)
getUserByMask mask = run_db (oneUser (sqlbind query [bindP mask]))
  where query = "SELECT userid,username,userpassword,ircmask,admin FROM user "
             ++ "WHERE ircmask = ?"

type UserIter x = Int -> String -> String -> Maybe String -> Int -> x

oneUser stmt = doQuery stmt iter Nothing
  where
  iter :: Monad m => UserIter (IterAct m (Maybe User))
  iter i a b c d _ = oneResult $ Just $ User i a b c (d == 1)

oneResult :: Monad m => IterAct m a
oneResult a = return (Left a)

storeSessionVar :: Show a => SessionId -> String -> a -> StoreM ()
storeSessionVar = undefined

getSessionVar :: Read a => SessionId -> String -> StoreM (Maybe a)
getSessionVar = undefined
