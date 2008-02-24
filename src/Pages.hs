{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pages where

import Text.XHtml.Strict hiding (URL)
import Text.Highlighting.Kate
import Data.Time
import MonadLib

import API
import Config
import Storage
import Types
import Utils.URL


data PageEnv = PageEnv { page_env_conf :: Config
                       , page_env_baseurl :: String }

newtype PageM a = PageM (ReaderT PageEnv Id a)
 deriving (Monad, Functor)


runPageM conf baseurl (PageM m) = runId $ runReaderT (PageEnv conf baseurl) m
asks f = f `fmap` PageM ask

make_url :: URL -> PageM String
make_url url = do b <- asks page_env_baseurl
                  return $ b ++ exportURL url


list_page :: [Paste] -> Int -> PageM Html
list_page pastes offset =
  mapM (make_url . methodURL mView . paste_id) pastes >>= \ urls ->
  make_url (methodURL mList Nothing (Just (offset + 1))) >>= \ earlier_url ->
  make_url (methodURL mList Nothing (Just (offset - 1))) >>= \ later_url ->
  skin "Recent Pastes" noHtml $
  h2 << "Recent Pastes"
 +++
  table ! [theclass "pastelist"]
  << (table_header
  +++ concatHtml
       [tr
        << (td << anchor ! [href view_url] << "view"
        +++ td << show_author p
        +++ td << show_title p
        +++ td << show_language p
        +++ td << paste_channel p
           )
       | (p,view_url) <- zip pastes1 urls]
     )
 +++
  thediv ! [theclass "footerlinks"]
  << (thespan ! [identifier "later"] << later_link later_url
  +++ " "
  +++ thespan ! [identifier "earlier"] << earlier_link earlier_url)
  where
  (pastes1, more) = splitAt 20 pastes

  earlier_link url
    | null more = toHtml "earlier"
    | otherwise = anchor ! [href url] << "earlier"

  later_link url
    | offset > 0 = anchor ! [href url] << "later"
    | otherwise  = toHtml "later"

  table_header = th << spaceHtml
             +++ th << "Author"
             +++ th << "Title"
             +++ th << "Language"
             +++ th << "Channel"

edit_paste_form :: [String] -> Maybe Int -> String -> PageM Html
edit_paste_form chans mb_pasteId starting_text = skin page_title noHtml $
  h2 << page_title
 +++
  form ! [action "save", method "post"]
  << (thediv ! [theclass "tabsrow1"]
      << (label ! [thefor "author"]
          << (thespan << "author "
          +++ input ! [ name "author", identifier "author", thetype "text" ]
             )
      +++ label ! [thefor "title"]
           << (thespan << "title " +++ textfield "title")

      +++ input ! [ thetype "submit", alt "save" ,theclass "imagebutton"
                  , name "submit", value "save" ]
          )
  +++ thediv ! [theclass "tabsrow2"]
      << (label ! [thefor "language"]
          << (thespan << "language " +++ language_drop_down)
      +++ label ! [thefor "channel"]
          << (thespan << "channel " +++ channel_drop_down)
         )
  +++ label ! [thefor "content"]
      << ("content "
      +++ textarea ! [rows "24", cols "80",identifier "content",name "content"]
          << starting_text
         )
  +++ parent_field
      )

  where
  page_title = case mb_pasteId of Just _ -> "New Revision"
                                  Nothing -> "New Paste"

  language_drop_down = select ! [name "language", identifier "language"]
                       << (option ! [value ""] << "Plain text"
                       +++ map language_option languages
                          )

  language_option "Haskell" = option ! [selected] << "Haskell"
  language_option l         = option << l

  channel_drop_down = select ! [name "channel", identifier "channel"]
                      << (option << emphasize << "none"
                      +++ map (option <<) chans
                         )

  parent_field = case mb_pasteId of
                   Just pasteId -> hidden "parent" (show pasteId)
                   Nothing      -> noHtml

display_pastes :: UTCTime -> Paste -> [Paste] -> PageM Html
display_pastes now x xs =
  make_url (methodURL mNew (Just (paste_id x)) Nothing) >>= \ new_url ->
  skin ("Viewing " ++ show_title x) (anchor ! [href new_url] << "add revision")
  . toHtml =<< mapM (display_paste now) (x:xs)
  where

display_paste :: UTCTime -> Paste -> PageM Html
display_paste now paste =
  make_url (methodURL mNew (Just (paste_id paste)) (Just ())) >>= \ new_url ->
  make_url (methodURL mRaw (paste_id paste)) >>= \ raw_url ->
  return $
      h2 << paste_title paste
  +++ thediv ! [theclass "entrylinks"]
      << (anchor ! [ href new_url] << "add modification"
      +++ anchor ! [href raw_url] << "raw"
         )
  +++ style ! [thetype "text/css"]
      << defaultHighlightingCss
  +++ thediv ! [theclass "labels"]
      << (make_label "author" (paste_author paste)
      +++ make_label "age" (show_ago now (paste_timestamp paste))
      +++ make_label "language" (show_language paste)
         )
  +++ thediv ! [theclass "contentbox"] << content

  where
  make_label k v = thespan ! [theclass "labelitem"]
                   << (thespan ! [theclass "labelkey"] << k
                   +++ thespan ! [theclass "labelvalue"] << v)

  content
    | null (paste_language paste) = pre ! [theclass "plaintext"]
                                    << paste_content paste
    | otherwise = case highlightAs (paste_language paste)
                                   (paste_content paste) of
                    Left e -> pre << e
                    Right ls -> formatAsXHtml [OptNumberLines]
                                    (paste_language paste) ls


skin :: String -> Html -> Html -> PageM Html
skin title_text other_links body_html =
  asks (style_path . page_env_conf) >>= \ stylesheet ->
  make_url (methodURL mList Nothing Nothing) >>= \ list_url ->
  make_url (methodURL mNew Nothing Nothing) >>= \ new_url ->
  return $

  header
  << (thetitle << (title_text ++ " - hpaste")
  +++ thelink ! [rel "stylesheet", thetype "text/css", href stylesheet]
      << noHtml
  +++ meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"]
     )

  +++
  body
  << (h1 << thespan << "hpastetwo"
  +++ thediv ! [theclass "toplinks"]
      << (anchor ! [href list_url] << "recent"
      +++ anchor ! [href new_url] << "new"
      +++ other_links
         )
  +++ body_html
     )
  +++
  thediv ! [theclass "footer"]
  << "hpaste 2008 - haskell-based web devel"

show_id :: Paste -> String
show_id p     = show $ paste_id p

show_author :: Paste -> String
show_author p | paste_author p == "" = "(anonymous)"
              | otherwise            = paste_author p

show_title :: Paste -> String
show_title p | paste_title p == "" = "(untitled)"
             | otherwise           = paste_title p

show_language :: Paste -> String
show_language p | paste_language p == "" = "Plain"
                | otherwise              = paste_language p

show_ago :: UTCTime -> Maybe UTCTime -> String
show_ago now Nothing = "unknown"
show_ago now (Just earlier) =
  let d = truncate $ diffUTCTime now earlier
  in if d == 0 then "new" else
     if d == 1 then "1 second" else
     if d < 60 then show d ++ " seconds" else
     if d < 120 then "1 minute" else
     if d < 3600 then show (d `div` 60) ++ " minutes" else
     if d < 7200 then "1 hour" else
     if d < 86400 then show (d `div` 3600) ++ " hours" else
     if d < 172800 then "1 day" else show (d `div` 86400) ++ " days"
