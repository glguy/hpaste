--------------------------------------------------------------------
-- |
-- Module    : Main
-- Copyright : (c) Eric Mertens
-- License   : BSD3
--
-- Maintainer: Eric Mertens <emertens@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Main(main) where

import API
import Config
import Highlight
import Pages
import PasteM
import Storage
import Types
import Utils.Compat()
import Utils.Misc
import Utils.URL

import qualified Codec.Binary.UTF8.Generic as UTF8
import Control.Concurrent
import Control.Monad (unless)
import Control.Exception
import Data.List
import Data.Time.Clock
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Network.FastCGI
import Network
import Prelude hiding (catch)
import System.IO
import Text.XHtml.Strict

handlers :: [Context -> Maybe (Either String Action)]
docs     :: [String]
(handlers,docs) = unzip
  [ mNew  --> handleNew
  , mSave --> handleSave
  , mView --> handleView
  , mRaw  --> handleRaw
  , mList --> handleList
  , mAddAnnot --> handleAddAnnot
  , mDelAnnot --> handleDelAnnot
  , mAnnotCss --> handleAnnotCss
  ]

usage :: String
usage = unlines $ intersperse "" docs

main :: IO ()
main =
 do pyh  <- init_highlighter
    conf <- loadConfig
    runFastCGIConcurrent' forkIO 10 (mainCGI pyh conf)

mainCGI :: PythonHandle -> Config -> CGI CGIResult
mainCGI pyh conf =
 do method     <- requestMethod
    params     <- getDecodedInputs
    p          <- drop 1 `fmap` pathInfo
    let c = Context method p params
    runPasteM pyh conf $ case runAPI c handlers of
      Nothing         -> outputHTML $ return $ pre << usage
      Just (Left err) -> outputHTML $ return $ pre << err
      Just (Right r)  -> r
  `catchCGI` outputException



-- | Handler for the edit form. This form is used for both new pastes
--   and revisions of existing ones.
handleNew :: Maybe Int -> Maybe () -> Action
handleNew mb_pasteId edit =
 do chans <- exec_db getChannels
    mb_text <- get_previous
    langs <- exec_python get_languages
    log_on_error mb_text $ \ (text, language) ->
      outputHTML $ edit_paste_form chans mb_pasteId language text langs
  where
  get_previous =
    case mb_pasteId of
      Nothing -> do lang <- default_language `fmap` get_conf
                    return $ Right ("",lang)
      Just x ->
        do res <- exec_db $ getPaste x
           return $
             case res of
               Nothing -> Left "no such paste"
               Just r | isNothing edit -> Right ("", paste_language r)
                      | otherwise -> Right $ (paste_content r,paste_language r)

-- | Handle saving of new pastes and revisions
--   XXX: Preview not supported yet
handleSave :: String -> String -> String -> String -> String -> Maybe Int
           -> Maybe () -> Action
handleSave title author content language channel mb_parent preview =
  exec_python get_languages >>= \ languages ->
  let validation_msgs = catMaybes [length_check "title" 40 title
                                  ,length_check "author" 40 author
                                  ,length_check "content" 5000 content
                                  ,blank_check "content" content
                                  ,member_check "language" language
                                                   (map snd languages)
                                  ]
  in if not (null validation_msgs)
        then outputHTML $ error_page validation_msgs
        else do
  mb_parent1 <- exec_db $ topmost_parent mb_parent
  chans <- exec_db getChannels
  let channel1 = if channel `elem` chans then channel else ""
  ip <- remoteAddr
  hostname <- remoteHost
  let paste = Paste { paste_id          = 0
                    , paste_title       = title
                    , paste_author      = author
                    , paste_content     = content
                    , paste_language    = language
                    , paste_channel     = channel1
                    , paste_parentid    = mb_parent1
                    , paste_hostname    = hostname
                    , paste_ipaddress   = ip
                    -- overwritten:
                    , paste_timestamp   = Nothing
                    , paste_expireon    = Nothing
                    }
  case preview of
    Just () -> do htm <- exec_python $ highlight 0 language content
                  outputHTML $ display_preview paste htm
    Nothing -> do pasteId <- exec_db $ writePaste paste
                  unless (null channel1) $ announce pasteId

                  -- now generate RSS
                  {-
                  Config { pastes_per_page = n
                         , base_url        = url
                         , rss_path        = path } <- get_conf
                  n      <- pastes_per_page `fmap` get_conf
                  url    <- base_url `fmap` get_conf
                  pastes <- exec_db $ getPastes Nothing n 0
                  liftIO $ forkIO $ outputRSS pastes url path
                  -}

                  redirectToView pasteId mb_parent1

-- | Write the id of a newly created paste to the socket to communicate to
--   the bot
announce :: Int -> PasteM ()
announce pasteId =
 do sockname <- announce_socket `fmap` get_conf
    liftIO $ (bracket (connectTo "" $ UnixSocket sockname)
                       hClose $ \ h ->
                 hPutStrLn h $ show pasteId
                 ) `catch` \ _ -> return ()

-- | Handle the viewing of existing pastes.
handleView :: Int -> Action
handleView pasteId =
 do res <- exec_db $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> do kids <- exec_db $ getChildren pasteId
                    let mb_last_mod = paste_timestamp (last (x:kids))
                    with_cache mb_last_mod $
                     do now <- liftIO getCurrentTime
                        xs <- mapM hl (x:kids)
                        outputHTML $ display_pastes now x kids xs
  where
  hl paste = exec_python $ highlight (paste_id paste)
                                     (paste_language paste)
                                     (paste_content paste)

handleAnnotCss :: Int -> Action
handleAnnotCss pasteId =
 do as <- exec_db $ getAnnotations pasteId
    setHeader "Content-type" "text/css; charset=utf-8"
    let names = [ concat ["#li-", show p, "-", show q] | (p, q) <- as]
    output $ concat (intersperse "," names) ++ "{background-color:yellow;}"

-- | Display a plain-text version of the paste. This is useful for downloading
--   the code.
handleRaw :: Int -> Action
handleRaw pasteId =
 do res <- exec_db $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> with_cache (paste_timestamp x) $
                  do setHeader "Content-type" "text/plain; charset=utf-8"
                     output $ UTF8.fromString $ paste_content x


-- | Display the most recent pastes. The number of pastes to display is set
--   in the configuration file.
handleList :: Maybe String -> Maybe Int -> Action
handleList pat offset =
 do let offset1 = max 0 $ fromMaybe 0 offset
    n      <- pastes_per_page `fmap` get_conf
    pastes <- exec_db $ getPastes pat (n+1) (offset1 * n)
    now    <- liftIO $ getCurrentTime
    outputHTML $ list_page now pastes pat offset1

-- | Mark lines in a paste to be highlighted
handleAddAnnot :: Int -> [(String,Int)] -> Action
handleAddAnnot pid ls = do mbPaste <- exec_db $ getPaste pid
                           case mbPaste of
                             Nothing -> outputNotFound $ "no paste " ++ show pid
                             Just paste ->
                               do exec_db (addAnnotations pid (map snd ls))
                                  redirectToView pid (paste_parentid paste)

-- | Unmark lines in a paste to be highlighted
handleDelAnnot :: Int -> [(String,Int)] -> Action
handleDelAnnot pid ls = do mbPaste <- exec_db $ getPaste pid
                           case mbPaste of
                             Nothing -> outputNotFound $ "no paste " ++ show pid
                             Just paste ->
                               do exec_db (delAnnotations pid (map snd ls))
                                  redirectToView pid (paste_parentid paste)

-- | Redirect the user to view another paste using 303 See Other
redirectToView :: MonadCGI m => Int -> Maybe Int -> m CGIResult
redirectToView pid mbPpid =
  do sn <- scriptName
     let url = methodURL mView $ fromMaybe pid mbPpid
     setStatus 303 "See Other"
     redirect $ sn ++ exportURL url ++ "#a" ++ show pid

-- | Given a value that could contain an error message, either report an
--   500 Internal Server Error or pass the result to the continuation.
log_on_error :: Either String a -> (a -> Action) -> Action
log_on_error (Right x) f = f x
log_on_error (Left  e) _ = outputInternalServerError [e]

-- | Ensure that a parameter is not left blank. Return Just error_message
--   when the parameter is blank.
blank_check :: String -> [a] -> Maybe Html
blank_check field_name xs
  | null xs   = Just $ emphasize << field_name +++ " is a required field."
  | otherwise = Nothing

-- | Ensure that a parameter is not longer than a maximum. Return
--   Just error_message when the parameter is too long.
length_check :: String -> Int -> [a] -> Maybe Html
length_check field_name n xs
  | length xs > n = Just $ emphasize << field_name
                      +++ " must not be longer than "
                      +++ strong << show n +++ " chacters."
  | otherwise     = Nothing

-- | Ensure that a parameter is a member of the given list.
--   Just error_message when the parameter is not a member.
member_check :: Eq a => String -> a -> [a] -> Maybe Html
member_check field_name x xs
  | x `elem` xs = Nothing
  | otherwise   = Just $ emphasize << field_name +++ " is not valid."

-- | Decode the UTF-8 bytes from the CGI inputs
getDecodedInputs = map decoder `fmap` getInputsFPS
  where decoder (x,y) = (UTF8.toString x, UTF8.toString y)

outputNotModified :: Action
outputNotModified = setStatus 304 "Not Modified" >> outputNothing

with_cache :: Maybe UTCTime -> Action -> Action
with_cache Nothing m = m
with_cache (Just last_mod) m =
 do last_mod_since <- requestHeader "If-Modified-Since"
    case read_rfc1123 =<< last_mod_since of
      Just t | t >= last_mod -> outputNotModified
      _ -> do setHeader "Last-Modified" (show_rfc1123 last_mod)
              m

