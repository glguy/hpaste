--------------------------------------------------------------------
-- |
-- Module    : hpaste
-- Copyright : (c) PDX Hackers, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Main where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad
import Data.Char
import Network.FastCGI
import Network.URI
import Database.Enumerator
import Database.Sqlite.Enumerator
import Text.XHtml.Strict

main = runFastCGIConcurrent' forkIO 5 mainCGI

mainCGI =
 do uri <- requestURI
    let p = split '/' $ uriPath uri
    handle (drop 3 p)

dbConnect = connect "pastes.db"

handle ["new"] =
  output $ edit_paste_form

handle [ps] | not (null ps) && all isDigit ps =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> output "no such paste"
      Just x  -> output $ display_paste x
 where pasteId = read ps

handle ["save"] =
 do title   <- getInput "title"
    author  <- getInput "author"
    content <- getInput "content"
    save    <- getInput "submit"
    case liftM3 (,,) title author content of
      Nothing -> output "missing inputs"
      Just (t,a,c) -> do
        mbPasteId <- liftIO $ writePaste t a c
        case mbPasteId of
          Right pasteId -> redirect (show pasteId)
          Left e -> output $ "Error: " ++ e

handle ps = do
    pastes <- liftIO $ getPastes
    output (listPage (show ps) pastes)

getPastes =
 withSession dbConnect $
   let query = sql "select pasteid, title from paste order by createstamp DESC"

       iter :: Monad m => Int -> String -> IterAct m [(Int, String)]
       iter a b acc = result' ( (a,b) : acc )

   in reverse `fmap` doQuery query iter []

getPaste :: Int -> IO (Maybe (String, String, String, String))
getPaste pasteId =
    withSession dbConnect $
    withPreparedStatement (prepareQuery query) $ \ pstmt ->
    withBoundStatement pstmt [bindP pasteId]   $ \ bstmt ->
        doQuery bstmt iterFirst Nothing

 where query = sql $ "select title,author,content,createstamp from paste" ++
                   " where pasteid = ?"

       iterFirst :: Monad m => String -> String -> String -> String
                 -> IterAct m (Maybe (String, String, String, String))

       iterFirst a b c d _ = return $ Left $ Just (a,b,c,d)

writePaste :: String -> String -> String -> IO (Either String Int)
writePaste title author content =
  let query1 = sqlbind "insert into paste (title, author, content) values (?,?,?)" bindings
      query2 = sql "select last_insert_rowid()"

      iterFirst :: Monad m => Int -> IterAct m (Either String Int)
      iterFirst i _ = return $ Left $ Right i

      bindings = [bindP title, bindP author, bindP content]
  in (
  withSession dbConnect $ do
  execDML query1
  return (Right 0)
  ) `catchDyn` \ (e :: DBException) -> return (Left (show e))
listPage ps pastes = renderHtml $
  p << ps
  +++ p << show pastes
  +++ unordList (map (\ (i,t) -> toHtml $ show i ++ ": " ++ t) pastes)

edit_paste_form = renderHtml $
  form ! [action "save", method "post"]
  << ( label ! [thefor "title"] << "Title: "
   +++ textfield "title"
   +++ br
   +++ label ! [thefor "author"] << "Author: "
   +++ textfield "author"
   +++ br
   +++ label ! [thefor "content"] << "Content:"
   +++ br
   +++ textarea ! [name "content"] << noHtml
   +++ br
   +++ submit "submit" "Publish"
     )

display_paste (title, author, content, create_date) = renderHtml $
      h1 << title
  +++ p << ("Author: " ++ author)
  +++ p << ("Date: " ++ create_date)
  +++ p << content

split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b
