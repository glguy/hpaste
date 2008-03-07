{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Storage
  (
  -- * The Store monad
    StoreM()
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
  , storeSessionData
  , retrieveSessionData
  ) where

import Types
import Utils.Misc(parse_time)

import Control.Monad (forM_)
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Database.Sqlite.Enumerator

newtype StoreM a = SM { unSM :: forall mark . DBM mark Session a }

instance Monad StoreM where
  SM m >>= f   = SM (m >>= unSM . f)
  SM m >> SM n = SM (m >> n)
  return x     = SM (return x)
  fail x       = SM (fail x)

runStoreM :: Typeable a => FilePath -> StoreM a -> IO a
runStoreM db (SM m) = withSession (connect db) m
                      `catchDB` \ e -> fail (formatDBException e)

execMany a bs = SM (withPreparedStatement (prepareCommand (sql a)) (\ p ->
                        forM_ bs (\ b -> withBoundStatement p b execDML)))

------------------------------------------------------------------------
-- Reading from the database

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset = SM (allPastes (sqlbind query bindings))
  where
  query = "SELECT * FROM paste WHERE " ++ cond ++
          "ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  bindings = param ++ [bindP limit, bindP offset]

  (param,cond) =
    case mpat of
      Just pat -> let xs = patternToQuery pat
                      qs = map (const "content LIKE ? ESCAPE '\\' ") xs
                  in (xs,concat (intersperse "AND " qs))
      Nothing -> ([], "parentid IS NULL ")

patternToQuery pat = map (\x -> bindP ("%"++ escape x ++ "%")) (my_words pat)
  where
  escape = concatMap escapeAux
  escapeAux '\\' = "\\\\"
  escapeAux '%'  = "\\%"
  escapeAux '_'  = "\\_"
  escapeAux x    =  [x]

  my_words = my_words1 . dropWhile isSpace

  my_words1 []       = []
  my_words1 ('"':xs) = let (a,b) = break (=='"') xs
                       in a : my_words (drop 1 b)

  my_words1 xs       = let (a,b) = break isSpace xs
                       in a : my_words b

getChildren :: Int -> StoreM [Paste]
getChildren parentid = SM (allPastes (sqlbind query [bindP parentid]))
  where
  query = "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = SM (onePaste (sqlbind query [bindP pasteId]))
  where query = "SELECT * FROM paste WHERE pasteid = ?"

getAnnotations :: Int -> StoreM [(Int,Int)]
getAnnotations pasteId = SM (allAnnotations (sqlbind query bindings))
  where
  query = "SELECT a.pasteid, a.line FROM annotation as a "
       ++ "INNER JOIN paste as p ON p.pasteid = a.pasteid "
       ++ "WHERE p.pasteid = ? OR p.parentid = ?"
  bindings = [bindP pasteId, bindP pasteId]

------------------------------------------------------------------------
-- Deleting from the database

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] = SM (execDML (cmdbind query [bindP pid]) >>
                                return ())
  where query = "DELETE FROM annotation WHERE pasteid = ?"

delAnnotations pid ls = execMany query binds
  where
  query = "DELETE FROM annotation WHERE pasteid = ? AND line = ?"
  binds = [ [bindP pid,bindP l] | l <- ls ]

------------------------------------------------------------------------
-- Adding annotations

addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls = execMany query binds
  where
  query = "REPLACE INTO annotation (pasteid,line) VALUES (?,?)"
  binds = [ [bindP pid, bindP l] | l <- ls ]

------------------------------------------------------------------------
-- Writing a new paste

writePaste :: Paste -> StoreM Int
writePaste p = SM (execDML (cmdbind query bindings) >>
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

------------------------------------------------------------------------

getChannels :: StoreM [String]
getChannels = SM (allChannels query)
  where query = "SELECT channelname from channel ORDER BY channelname"


addChannel :: String -> StoreM Int
addChannel chan = SM (execDML (cmdbind query [bindP chan]))
  where query = "INSERT INTO channel (channelname) VALUES (?)"

delChannel :: String -> StoreM Int
delChannel chan = SM (execDML (cmdbind query [bindP chan]))
  where query = "DELETE FROM channel WHERE channelname = ?"

clearChannels :: StoreM Int
clearChannels = SM (execDML "DELETE FROM channel")

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ fromMaybe (paste_id ppaste) (paste_parentid ppaste)
  where bind m f = maybe (return Nothing) f =<< m


allAnnotations stmt = doQuery stmt iter []
  where
  iter :: Monad m => Int -> Int -> IterAct m [(Int,Int)]
  iter x y acc = result' ((x,y) : acc)

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
getUserByMask mask = SM (oneUser (sqlbind query [bindP mask]))
  where query = "SELECT userid,username,userpassword,ircmask,admin FROM user "
             ++ "WHERE ircmask = ?"

type UserIter x = Int -> String -> String -> Maybe String -> Int -> x

oneUser stmt = doQuery stmt iter Nothing
  where
  iter :: Monad m => UserIter (IterAct m (Maybe User))
  iter i a b c d _ = oneResult $ Just $ User i a b c (d == 1)

oneResult :: Monad m => IterAct m a
oneResult a = return (Left a)

storeSessionData sid dat = return ()

retrieveSessionData sid = return (SessionData Nothing)
