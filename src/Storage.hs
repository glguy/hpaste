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

import Database.Sqlite.Enumerator
import Types
import Utils.Misc(parse_time)

import MonadLib

newtype StoreM a = SM (ReaderT FilePath IO a) deriving (Functor,Monad)

get_db :: StoreM FilePath
get_db = SM ask

runStoreM :: FilePath -> StoreM a -> IO a
runStoreM db (SM m) = runReaderT db m

data Handler bstmt s a  = Handler (forall mark. bstmt -> DBM mark s a)
data StmtH st s a       = StmtH (forall m. PreparedStmt m st -> DBM m s a)

with_statement query (StmtH f) =
  get_db >>= \db -> SM $ lift (
    withSession (connect db) (
      withPreparedStatement (prepareQuery (sql query)) f
    )
    `catchDB` \e -> ioError (userError (show e))
  )

with_db (query,bindings,Handler f) =
  with_statement query $
    StmtH (\pstmt -> withBoundStatement pstmt bindings f)

try_exec_dml s = (execDML s >> return ()) `catchDB` \_ -> return ()

getPastes :: Maybe String -> Int -> Int -> StoreM [Paste]
getPastes mpat limit offset = with_db
  ("SELECT * FROM paste" ++ cond
                         ++ " ORDER BY createstamp DESC LIMIT ? OFFSET ?"
  , param ++ [bindP limit, bindP offset]
  , Handler (\bstmt -> reverse `fmap` doQuery bstmt iterAll [])
  )
  where
  (param,cond) = case mpat of
                   Just pat -> ([bindP pat]," WHERE content LIKE ?")
                   Nothing -> ([], " WHERE parentid IS NULL")

getChildren :: Int -> StoreM [Paste]
getChildren parentid = with_db
  ( "SELECT * from paste WHERE parentid = ? ORDER BY createstamp ASC"
  , [bindP parentid]
  , Handler (\bstmt -> reverse `fmap` doQuery bstmt iterAll [])
  )

getPaste :: Int -> StoreM (Maybe Paste)
getPaste pasteId = with_db
  ( "SELECT * FROM paste WHERE pasteid = ?"
  , [bindP pasteId]
  , Handler (\bstmt -> doQuery bstmt iterFirst (Nothing :: Maybe Paste))
  )

getAnnotations :: Int -> StoreM [Int]
getAnnotations pasteId = with_db
  ( "SELECT line FROM annotation WHERE pasteid = ?"
  , [bindP pasteId]
  , Handler (\s -> doQuery s collect_field_all [])
  )

-- | The empt ylist of lines means "remove all annotations"!
delAnnotations :: Int -> [Int] -> StoreM ()
delAnnotations pid [] = with_db
  ( "DELETE FROM annotation WHERE pastedid = ?"
  , [bindP pid]
  , Handler try_exec_dml
  )

delAnnotations pid ls = with_statement query $
  StmtH (\s -> mapM_ (\args -> withBoundStatement s args try_exec_dml) binds)
  where
  query = "DELETE FROM annotation WHERE pastedid = ? AND line = ?"
  binds = [ [bindP pid,bindP l] | l <- ls ]


addAnnotations :: Int -> [Int] -> StoreM ()
addAnnotations pid ls = with_statement query $
  StmtH (\s -> mapM_ (\args -> withBoundStatement s args try_exec_dml) binds)
  where
  query = "INSERT INTO annotation (pasteid,line) VALUES (?,?)"
  binds = [ [bindP pid,bindP l] | l <- ls ]



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

writePaste :: Paste -> StoreM (Either String Int)
writePaste p = with_db
  ( "INSERT INTO paste "
 ++ "(title, author, content, language, channel, parentid, ipaddress, hostname)"
 ++ " VALUES (?,?,?,?,?,?,?,?)"
  , [ bindP (paste_title p), bindP (paste_author p)
    , bindP (paste_content p), bindP (paste_language p)
    , bindP (paste_channel p), bindP (paste_parentid p)
    , bindP (paste_ipaddress p), bindP (paste_hostname p)
    ]
  , Handler (\bstmt -> do execDML bstmt
                          (Right . fromIntegral) `fmap` inquire LastInsertRowid
                        `catchDB` \ e -> return (Left (show e))
            )
  )
  where
  iterFirst :: Monad m => Int -> IterAct m (Either String Int)
  iterFirst i _ = return $ Left $ Right i

getChannels :: StoreM [String]
getChannels = with_db
  ( "SELECT channelname from channel ORDER BY channelname"
  , []
  , Handler (\bstmt -> reverse `fmap` doQuery bstmt collect_field_all [])
  )

-- Collect all from a single field.
collect_field_all :: Monad m => a -> IterAct m [a]
collect_field_all x xs = result' (x:xs)

addChannel :: String -> StoreM ()
addChannel chan = with_db
  ( "INSERT INTO channel (channelname) VALUES (?)"
  , [bindP chan]
  , Handler (\bstmt -> (execDML bstmt >> return ()) `catchDB` \ _ -> return ())
  )

delChannel :: String -> StoreM ()
delChannel chan = with_db
  ( "DELETE FROM channel WHERE channelname = ?"
  , [bindP chan]
  , Handler (\bstmt -> (execDML bstmt >> return ()) `catchDB` \ _ -> return ())
  )

clearChannels :: StoreM ()
clearChannels = with_db
  ( "DELETE FROM channel"
  , []
  , Handler (\bstmt -> (execDML bstmt >> return ()) `catchDB` \ _ -> return ())
  )

topmost_parent :: Maybe Int -> StoreM (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ case paste_parentid ppaste of
                    Just i | i > 0 -> i
                    _ -> paste_id ppaste

  where bind m f = maybe (return Nothing) f =<< m
