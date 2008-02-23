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
import Types
import Utils.URL

import Control.Concurrent
import Control.Monad (unless)
import Control.Exception
import Data.Char
import Data.Maybe (fromMaybe, isNothing)
import Network.FastCGI
import Network.URI
import Network
import Prelude hiding (catch)
import System.IO
import Text.XHtml.Strict hiding (URL)

handlers :: [Context -> Maybe (Either String (CGIT IO CGIResult))]
docs     :: [String]
(handlers,docs) = unzip
  [ mNew  --> handleNew
  , mSave --> handleSave
  , mView --> handleView
  , mRaw  --> handleRaw
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

handleNew :: Maybe Int -> Maybe () -> CGI CGIResult
handleNew mb_pasteId edit =
 do chans <- liftIO getChannels
    mb_text <- get_text
    log_on_error mb_text $ \ text ->
      outputHTML $ edit_paste_form chans mb_pasteId text
  where
  get_text =
    if isNothing edit then return $ Right ""
    else case mb_pasteId of
           Nothing -> return $ Right ""
           Just x  -> do res <- liftIO $ getPaste x
                         return $ case res of
                                    Just r -> Right $ paste_content r
                                    Nothing -> Left "no such paste"

handleSave :: String -> String -> String -> String -> String -> Maybe Int
           -> Maybe () -> Maybe () -> CGI CGIResult
handleSave title author content language channel mb_parent save preview = do
  mb_parent1 <- case mb_parent of
                  Nothing -> return Nothing
                  Just parent ->
                    do ppaste <- liftIO $ getPaste parent
                       case ppaste of
                         Nothing -> return Nothing
                         Just x -> return $ Just $ fromMaybe (paste_id x)
                                                             (paste_parentid x)
  let paste = Paste { paste_id = 0
                    , paste_title = title
                    , paste_author = author
                    , paste_content = content
                    , paste_language = language
                    , paste_channel = channel
                    , paste_parentid = mb_parent1
                    }
  mbPasteId <- liftIO $ writePaste paste
  log_on_error mbPasteId $ \ pasteId -> do
    unless (null channel) $ liftIO $ announce pasteId
    redirectTo $ methodURL mView $ fromMaybe pasteId (mb_parent1)

announce :: Int -> IO ()
announce pasteId = (bracket (connectTo "" $ UnixSocket "pastes/announce")
                           hClose $ \ h -> hPutStrLn h $ show pasteId
                   ) `catch` \ _ -> return ()

handleView :: Int -> CGI CGIResult
handleView pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do kids <- liftIO $ getChildren (pasteId)
                    outputHTML $ display_pastes x kids

handleRaw :: Int -> CGI CGIResult
handleRaw pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do setHeader "Content-type" "text/plain"
                    output $ paste_content x


handleList :: CGI CGIResult
handleList = do
    pastes <- liftIO $ getPastes 50 0
    outputHTML $ list_page pastes

split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b

outputHTML :: HTML a => a -> CGI CGIResult
outputHTML s = do setHeader "Content-type" "text/html; charset=utf-8"
                  output (filter (/='\r') $ renderHtml s)

redirectTo :: URL -> CGI CGIResult
redirectTo url = redirect $ exportURL url

log_on_error :: Either String a -> (a -> CGI CGIResult) -> CGI CGIResult
log_on_error (Right x) f = f x
log_on_error (Left  e) _ = outputInternalServerError [e]
