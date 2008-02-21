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

import API
import Storage
import Utils.URL

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad
import Data.Char
import Network.FastCGI
import Network.URI
import Text.XHtml.Strict

(handlers,docs) = unzip
  [ mNew  --> handleNew
  , mSave --> handleSave
  , mView --> handleView
  , mList --> handleList
  ]

usage = unlines docs

main = runFastCGIConcurrent' forkIO 5 mainCGI

mainCGI =
 do uri    <- requestURI
    method <- requestMethod
    params <- getInputs
    let path = uriPath uri
    let c = Context method (reverse $ takeWhile (/= '/') $ reverse path) params
    case runAPI c handlers of
      Nothing         -> outputHTML $ pre << usage
      Just (Left err) -> outputHTML $ pre << err
      Just (Right r)  -> r

handleNew :: CGI CGIResult
handleNew = outputHTML edit_paste_form

handleSave :: String -> String -> String -> Maybe () -> Maybe ()
           -> CGI CGIResult
handleSave title author content save preview = do
  mbPasteId <- liftIO $ writePaste title author content
  case mbPasteId of
    Right pasteId -> redirect $ exportURL $ methodURL mView pasteId
    Left e -> output $ "Error: " ++ e

handleView :: Int -> CGI CGIResult
handleView pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> output "no such paste"
      Just x  -> outputHTML $ display_paste x

handleList = do
    pastes <- liftIO $ getPastes
    outputHTML $ list_page pastes

list_page pastes =
  p << show pastes
  +++ unordList (map (\ (i,t) -> toHtml $ show i ++ ": " ++ t) pastes)

edit_paste_form =
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

display_paste paste =
      h1 << show (paste_title paste)
  +++ p << ("Author: " ++ show (paste_author paste))
  +++ p << ("Date: " ++ show (paste_timestamp paste))
  +++ p << paste_content paste

split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b

outputHTML :: HTML a => a -> CGI CGIResult
outputHTML s = output (renderHtml s)
