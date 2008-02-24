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
import Config
import Utils.URL
import Utils.Misc(maybeRead)
import Utils.Compat()

import Control.Concurrent
import Control.Exception
import Data.Time.Clock
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Network.FastCGI
import Network.URI
import Network
import Prelude hiding (catch)
import System.IO
import Text.XHtml.Strict hiding (URL)
import Text.Highlighting.Kate (languages)
import MonadLib

type PasteM = ReaderT Config (CGIT IO)
type Action = PasteM CGIResult

get_conf :: PasteM Config
get_conf = ask

runPasteM :: Config -> PasteM a -> CGI a
runPasteM = runReaderT

handlers :: [Context -> Maybe (Either String Action)]
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
    conf   <- liftIO getConfig
    let p = uriPath uri
    let c = Context method (reverse $ takeWhile (/= '/') $ reverse p) params
    runPasteM conf $ case runAPI c handlers of
      Nothing         -> outputHTML $ return $ pre $ toHtml usage
      Just (Left err) -> outputHTML $ return err
      Just (Right r)  -> r
  `catchCGI` outputException

getConfig :: IO Config
getConfig =
  do txt <- readFile "hpaste.conf"
     case maybeRead txt of
       Just conf -> return conf
       Nothing -> return default_config
   `catch` \_ -> return default_config




handleNew :: Maybe Int -> Maybe () -> Action
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
           -> Maybe () -> Maybe () -> Action
handleSave title author content language channel mb_parent save preview =
  let validation_msgs = catMaybes [length_check "title" 40 title
                                  ,length_check "author" 40 author
                                  ,length_check "content" 5000 content
                                  ,blank_check "title" title
                                  ,blank_check "author" author
                                  ,blank_check "content" content
                                  ,member_check "language" language
                                                              ("":languages)
                                  ]
  in if not (null validation_msgs)
        then outputHTML $ error_page validation_msgs
        else do
  mb_parent1 <- liftIO $ topmost_parent mb_parent
  chans <- liftIO $ getChannels
  let channel1 = if channel `elem` chans then channel else ""
  ip <- remoteAddr
  hostname <- remoteHost
  let paste = Paste { paste_id = 0
                    , paste_title = title
                    , paste_author = author
                    , paste_content = content
                    , paste_language = language
                    , paste_channel = channel1
                    , paste_parentid = mb_parent1
                    , paste_hostname = hostname
                    , paste_ipaddress = ip
                    -- overwritten:
                    , paste_timestamp = Nothing
                    , paste_expireon = Nothing
                    }
  mbPasteId <- liftIO $ writePaste paste
  log_on_error mbPasteId $ \ pasteId -> do
    unless (null channel1) $ liftIO $ announce pasteId
    handleView (fromMaybe pasteId mb_parent1)

announce :: Int -> IO ()
announce pasteId = (bracket (connectTo "" $ UnixSocket "pastes/announce")
                           hClose $ \ h -> hPutStrLn h $ show pasteId
                   ) `catch` \ _ -> return ()

handleView :: Int -> Action
handleView pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do kids <- liftIO $ getChildren (pasteId)
                    now <- liftIO $ getCurrentTime
                    outputHTML $ display_pastes now x kids

handleRaw :: Int -> Action
handleRaw pasteId =
 do res <- liftIO $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do setHeader "Content-type" "text/plain"
                    output $ paste_content x


handleList :: Maybe String -> Maybe Int -> Action
handleList pat offset = do
    let offset1 = max 0 $ fromMaybe 0 offset
    n <- pastes_per_page `fmap` get_conf
    pastes <- liftIO $ getPastes pat (n+1) (offset1 * n)
    now <- liftIO $ getCurrentTime
    outputHTML $ list_page now pastes offset1

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d xs = case break (==d) xs of
               (a, []) -> [a]
               (a, _:b) -> a : split d b

buildHTML :: PageM a -> PasteM a
buildHTML m      = do sn <- scriptName
                      conf <- get_conf
                      return $ runPageM conf sn m

outputHTML :: HTML a => PageM a -> PasteM CGIResult
outputHTML s = do setHeader "Content-type" "text/html; charset=utf-8"
                  xs <- buildHTML s
                  output $ filter (/='\r') $ renderHtml xs

redirectTo :: URL -> CGI CGIResult
redirectTo url = do sn <- scriptName
                    redirect $ sn ++ exportURL url

log_on_error :: Either String a -> (a -> Action) -> Action
log_on_error (Right x) f = f x
log_on_error (Left  e) _ = outputInternalServerError [e]

blank_check :: String -> [a] -> Maybe Html
blank_check field_name xs
  | null xs   = Just $ emphasize << field_name +++ " is a required field."
  | otherwise = Nothing

length_check :: String -> Int -> [a] -> Maybe Html
length_check field_name n xs
  | length xs > n = Just $ emphasize << field_name
                      +++ " must not be longer than "
                      +++ strong << show n +++ " chacters."
  | otherwise     = Nothing

member_check :: Eq a => String -> a -> [a] -> Maybe Html
member_check field_name x xs
  | x `elem` xs = Nothing
  | otherwise   = Just $ emphasize << field_name +++ " is not valid."

topmost_parent :: Maybe Int -> IO (Maybe Int)
topmost_parent mb_parent =
  return mb_parent `bind` \ parent ->
  getPaste parent `bind` \ ppaste ->
  return $ Just $ case paste_parentid ppaste of
                    Just i | i > 0 -> i
                    _ -> paste_id ppaste

  where bind m f = maybe (return Nothing) f =<< m
