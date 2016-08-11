{-# LANGUAGE OverloadedStrings #-}

module Storage
{-
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

  -- * User functions
  , getUserByMask

  -- * Session functions
  , storeSessionData
  , retrieveSessionData
  )-} where

import Types

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple

newtype StoreM a = SM { unSM :: Connection -> IO a }

instance Functor StoreM where
  fmap = liftM

instance Applicative StoreM where
  pure x = SM (\_ -> pure x)
  (<*>) = ap

instance Monad StoreM where
  SM m >>= f   = SM (\conn -> do x <- m conn
                                 unSM (f x) conn)

instance FromRow Paste where
  fromRow = Paste <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = withConnection db m

querySM :: (ToRow p, FromRow r) => Query -> p -> StoreM [r]
querySM q p = SM(\conn -> query conn q p)

execSM :: ToRow p => Query -> p -> StoreM ()
execSM q p = SM (\conn -> execute conn q p)

------------------------------------------------------------------------
-- Reading from the database

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset =
  querySM (fromString querySql) bindings
  where
  querySql = "SELECT * FROM paste WHERE " ++ cond ++
          "ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  bindings = param :. (limit, offset)

  (param,cond) =
    case mpat of
      Just pat -> let xs = patternToQuery pat
                      qs = map (const "content LIKE ? ESCAPE '\\' ") xs
                  in (xs,concat (intersperse "AND " qs))
      Nothing -> ([], "parentid IS NULL ")

patternToQuery pat = map (\x -> ("%"++ escape x ++ "%")) (my_words pat)
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
getChildren parentid = querySM sql (Only parentid)
  where
  sql = "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = listToMaybe <$> querySM sql (Only pasteId)
  where sql = "SELECT * FROM paste WHERE pasteid = ?"

getAnnotations :: Int -> StoreM [(Int,Int)]
getAnnotations pasteId = querySM sql bindings
  where
  sql   = "SELECT a.pasteid, a.line FROM annotation as a\
          \ INNER JOIN paste as p ON p.pasteid = a.pasteid\
          \ WHERE p.pasteid = ? OR p.parentid = ?"
  bindings = (pasteId, pasteId)

------------------------------------------------------------------------
-- Deleting from the database

-- | The empty list of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] = execSM sql (Only pid)
  where sql = "DELETE FROM annotation WHERE pasteid = ?"

delAnnotations pid ls =
  forM_ ls $ \l ->
    execSM sql (pid, l)
  where
  sql = "DELETE FROM annotation WHERE pasteid = ? AND line = ?"

------------------------------------------------------------------------
-- Adding annotations

addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls =
  forM_ ls $ \l ->
     execSM sql (pid, l)
  where
  sql = "REPLACE INTO annotation (pasteid,line) VALUES (?,?)"

------------------------------------------------------------------------
-- Writing a new paste

writePaste :: Paste -> StoreM Int
writePaste p =
  do execSM sql bindings
     fromIntegral <$> SM lastInsertRowId
  where
  sql   = "INSERT INTO paste (title, author, content, language,\
          \ channel, parentid, ipaddress, hostname)\
          \ VALUES (?,?,?,?,?,?,?,?)"
  bindings =  ( paste_title p    , paste_author p
              , paste_content p  , paste_language p
              , paste_channel p  , paste_parentid p
              , paste_ipaddress p, paste_hostname p
              )

------------------------------------------------------------------------


topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ fromMaybe (paste_id ppaste) (paste_parentid ppaste)
  where bind m f = maybe (return Nothing) f =<< m
