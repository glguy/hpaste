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
import Pages
import Storage
import Utils.URL

import Control.Concurrent
import Data.Char
import Network.FastCGI
import Network.URI
import Text.XHtml.Strict (renderHtml, HTML())

handlers :: [Context -> Maybe (Either String (CGIT IO CGIResult))]
docs     :: [String]
(handlers,docs) = unzip
  [ mNew  --> handleNew
  , mSave --> handleSave
  , mView --> handleView
  , mList --> handleList
  ]

usage :: String
usage = unlines docs

main :: IO ()
main = runFastCGIConcurrent' forkIO 5 mainCGI

mainCGI :: CGIT IO CGIResult
mainCGI =
 do uri    <- requestURI
    method <- requestMethod
    params <- getInputs
    let p = uriPath uri
    let c = Context method (reverse $ takeWhile (/= '/') $ reverse p) params
    case runAPI c handlers of
      Nothing         -> outputHTML usage
      Just (Left err) -> outputHTML err
      Just (Right r)  -> r

handleNew :: CGI CGIResult
handleNew = outputHTML edit_paste_form

handleSave :: String -> String -> String -> Maybe () -> Maybe ()
           -> CGI CGIResult
handleSave title author content save preview = do
  mbPasteId <- liftIO $ writePaste title author content
  log_on_error mbPasteId $ \ pasteId ->
    redirectTo $ methodURL mView pasteId

handleView :: Int -> CGI CGIResult
handleView pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> outputHTML $ display_paste x

handleList :: CGI CGIResult
handleList = do
    pastes <- liftIO $ getPastes 50 0
    outputHTML $ list_page pastes

split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b

outputHTML :: HTML a => a -> CGI CGIResult
outputHTML s = output (renderHtml s)

redirectTo :: URL -> CGI CGIResult
redirectTo url = redirect $ exportURL url

log_on_error :: Either String a -> (a -> CGI CGIResult) -> CGI CGIResult
log_on_error (Right x) f = f x
log_on_error (Left  e) _ = outputInternalServerError [e]
