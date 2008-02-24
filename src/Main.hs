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
import Data.Time.Clock
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
      Nothing         -> outputHTML $ return $ pre $ toHtml usage
      Just (Left err) -> outputHTML $ return err
      Just (Right r)  -> r
  `catchCGI` outputException

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
                         Just x -> return $ Just $ case paste_parentid x of
                                                     Just i | i > 0 -> i
                                                     _ -> paste_id x
  ip <- remoteAddr
  hostname <- remoteHost
  let paste = Paste { paste_id = 0
                    , paste_title = title
                    , paste_author = author
                    , paste_content = content
                    , paste_language = language
                    , paste_channel = channel
                    , paste_parentid = mb_parent1
                    , paste_hostname = hostname
                    , paste_ipaddress = ip
                    -- overwritten:
                    , paste_timestamp = Nothing
                    , paste_expireon = Nothing
                    }
  mbPasteId <- liftIO $ writePaste paste
  log_on_error mbPasteId $ \ pasteId -> do
    unless (null channel) $ liftIO $ announce pasteId
    handleView $ fromMaybe pasteId mb_parent1

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
                    now <- liftIO $ getCurrentTime
                    outputHTML $ display_pastes now x kids

handleRaw :: Int -> CGI CGIResult
handleRaw pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do setHeader "Content-type" "text/plain"
                    output $ paste_content x


handleList :: Maybe String -> CGI CGIResult
handleList pat = do
    pastes <- liftIO $ getPastes pat 50 0
    outputHTML $ list_page pastes

split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b

buildHTML :: PageM a -> CGI a
buildHTML m = do sn <- scriptName
                 return $ runPageM sn m

outputHTML :: HTML a => PageM a -> CGI CGIResult
outputHTML s = do setHeader "Content-type" "text/html; charset=utf-8"
                  xs <- buildHTML s
                  output $ filter (/='\r') $ renderHtml xs

redirectTo :: URL -> CGI CGIResult
redirectTo url = do sn <- scriptName
                    redirect $ sn ++ exportURL url

log_on_error :: Either String a -> (a -> CGI CGIResult) -> CGI CGIResult
log_on_error (Right x) f = f x
log_on_error (Left  e) _ = outputInternalServerError [e]
