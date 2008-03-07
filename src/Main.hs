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

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Control.Concurrent
import Control.Exception
import Control.Monad (unless,liftM3)
import Data.List
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Time.Clock
import Network
import Network.FastCGI
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
 do c <- liftM3 Context requestMethod pathInfo getDecodedInputs
    runPasteM pyh conf $ case runAPI c handlers of
      Nothing         -> outputHTML $ return $ pre << usage
      Just (Left err) -> outputHTML $ return $ pre << err
      Just (Right r)  -> r
  `catchCGI` outputException



-- | Handler for the edit form. This form is used for both new pastes
--   and revisions of existing ones.
handleNew :: Maybe Int -> Maybe () -> Action
handleNew mb_pasteId edit =
 do chans   <- exec_db getChannels
    mb_text <- get_previous_content mb_pasteId (isJust edit)
    langs   <- exec_python get_languages
    log_on_error mb_text $ \ (text, language) ->
      outputHTML $ edit_paste_form chans mb_pasteId language text langs

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
  ip         <- remoteAddr
  hostname   <- remoteHost
  mb_parent1 <- exec_db $ topmost_parent mb_parent
  chans      <- exec_db getChannels
  let channel1 = if channel `elem` chans then channel else ""
      paste = Paste { paste_id          = 0
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
                  path   <- withConf rss_path
                  n      <- withConf pastes_per_page
                  url    <- withConf base_url
                  pastes <- exec_db $ getPastes Nothing n 0
                  liftIO $ forkIO $ outputRSS pastes url path
                  -}

                  redirectToView pasteId mb_parent1

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
    setContentType css_content_type
    let names = [ concat ["#li-", show p, "-", show q] | (p, q) <- as]
    output $ intercalate "," names ++ "{background-color:yellow;}"

-- | Display a plain-text version of the paste. This is useful for downloading
--   the code.
handleRaw :: Int -> Action
handleRaw pasteId =
 do res <- exec_db $ getPaste pasteId
    case res of
      Nothing -> outputNotFound $ "paste #" ++ show pasteId
      Just x  -> with_cache (paste_timestamp x) $
                  do setContentType plain_content_type
                     output $ UTF8.encodeString $ paste_content x


-- | Display the most recent pastes. The number of pastes to display is set
--   in the configuration file.
handleList :: Maybe String -> Maybe Int -> Action
handleList pat offset =
 do let offset1 = max 0 $ fromMaybe 0 offset
    n      <- withConf pastes_per_page
    pastes <- exec_db $ getPastes pat (n+1) (offset1 * n)
    now    <- liftIO $ getCurrentTime
    outputHTML $ list_page now pastes pat offset1

-- | Mark lines in a paste to be highlighted
handleAddAnnot :: Int -> [(String,Int)] -> Action
handleAddAnnot pid ls =
 do mbPaste <- exec_db $ getPaste pid
    case mbPaste of
      Nothing    -> outputNotFound $ "no paste " ++ show pid
      Just paste -> do exec_db (addAnnotations pid (map snd ls))
                       redirectToView pid (paste_parentid paste)

-- | Unmark lines in a paste to be highlighted
handleDelAnnot :: Int -> [(String,Int)] -> Action
handleDelAnnot pid ls =
 do mbPaste <- exec_db $ getPaste pid
    case mbPaste of
      Nothing -> outputNotFound $ "no paste " ++ show pid
      Just paste -> do exec_db (delAnnotations pid (map snd ls))
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
getDecodedInputs :: CGI [(String,String)]
getDecodedInputs = map decoder `fmap` getInputsFPS
  where decoder (x,y) = (UTF8.decodeString x, LazyUTF8.toString y)

outputNotModified :: Action
outputNotModified = setStatus 304 "Not Modified" >> outputNothing

with_cache :: Maybe UTCTime -> Action -> Action
with_cache Nothing         m = m
with_cache (Just last_mod) m =
 do last_mod_since <- requestHeader "If-Modified-Since"
    case read_rfc1123 =<< last_mod_since of
      Just t | t >= last_mod -> outputNotModified
      _ -> setHeader "Last-Modified" (show_rfc1123 last_mod) >> m

get_previous_content :: Maybe Int -> Bool
                     -> PasteM (Either String (String,String))
get_previous_content Nothing _ =
 do lang <- withConf default_language
    return $ Right ("",lang)
get_previous_content (Just x) edit =
 do res <- exec_db $ getPaste x
    return $ case res of
               Nothing            -> Left "no such paste"
               Just r | edit      -> Right $ (paste_content r,paste_language r)
                      | otherwise -> Right ("", paste_language r)

-- | Write the id of a newly created paste to the socket to communicate to
--   the bot
announce :: Int -> PasteM ()
announce pasteId =
 do sockname <- withConf announce_socket
    liftIO $ (bracket (connectTo "" $ UnixSocket sockname) hClose $ \ h ->
                hPutStrLn h $ show pasteId
             ) `catch` \ _ -> return ()
