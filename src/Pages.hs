{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pages where

import Text.XHtml.Strict hiding (URL)
import Text.Highlighting.Kate
import Data.Time
import MonadLib

import API
import Config
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

error_page :: [Html] -> PageM Html
error_page errors =
  skin "Validation problem" noHtml $
  h2 << "Paste validation failed"
  +++
  ordList errors
  +++
  p << "Please use your back button to correct your paste."

list_page :: UTCTime -> [Paste] -> Int -> PageM Html
list_page now pastes offset =
  mapM (make_url . methodURL mView . paste_id) pastes >>= \ urls ->
  asks (pastes_per_page . page_env_conf) >>= \ n ->
  make_url (methodURL mList Nothing (Just (offset + 1))) >>= \ earlier_url ->
  make_url (methodURL mList Nothing (Just (offset - 1))) >>= \ later_url ->
  skin "Recent Pastes" noHtml $ html_result urls n earlier_url later_url

  where
  html_result urls n earlier_url later_url =
     h2 << "Recent Pastes"
    +++
     table ! [theclass "pastelist"]
     << (table_header
     +++ concatHtml
          [tr
           << (td << show_author p
           +++ td << anchor ! [href view_url] << show_title p
           +++ td << show_ago now p
           +++ td << show_language p
           +++ td << paste_channel p
              )
          | (p,view_url) <- zip pastes1 urls]
        )
    +++
     thediv ! [theclass "footerlinks"]
     << (thespan ! [identifier "later"] << later_link
     +++ " "
     +++ thespan ! [identifier "earlier"] << earlier_link)
    where
    (pastes1, more) = splitAt n pastes

    earlier_link
      | null more = toHtml "earlier"
      | otherwise = anchor ! [href earlier_url] << "earlier"

    later_link
      | offset > 0 = anchor ! [href later_url] << "later"
      | otherwise  = toHtml "later"

    table_header = th << "Author"
               +++ th << "Title"
               +++ th << "Age"
               +++ th << "Language"
               +++ th << "Channel"

edit_paste_form :: [String] -> Maybe Int -> String -> String -> PageM Html
edit_paste_form chans mb_pasteId language starting_text =
  skin page_title noHtml $
  h2 << page_title
 +++
  form ! [action "save", method "post"]
  << (thediv ! [theclass "tabsrow1"]
      << (label ! [thefor "author"]
          << (thespan << "author "
          +++ input ! [ name "author", identifier "author", thetype "text"
                      , maxlength 40 ]
             )
      +++ label ! [thefor "title"]
           << (thespan << "title "
           +++ input ! [name "title", identifier "title", thetype "text"
                       ,maxlength 40])

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

  language_option l | l == language = option ! [selected] << l
                    | otherwise     = option << l

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
      style ! [thetype "text/css"]
      << defaultHighlightingCss
  +++ thediv ! [theclass "labels"]
      << (make_label "author" (paste_author paste)
      +++ make_label "age" (show_ago now paste)
      +++ make_label "language" (show_language paste)
         )
  +++ h2 << paste_title paste
  +++ thediv ! [theclass "entrylinks"]
      << (anchor ! [ href new_url] << "add modification"
      +++ anchor ! [href raw_url] << "raw"
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
  << (h1 << anchor ! [href list_url]
            << (thespan << "hpastetwo" +++ spaceHtml)
  +++ thediv ! [theclass "toplinks"]
      << (anchor ! [href list_url] << "recent"
      +++ anchor ! [href new_url] << "new"
      +++ other_links
         )
  +++ thediv ! [theclass "wrapper"] << body_html
  +++ thediv ! [theclass "footer"]
      << "hpaste 2008 - haskell-based web devel"
     )

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

show_ago :: UTCTime -> Paste -> String
show_ago now paste = helper (paste_timestamp paste)
  where
  helper Nothing = "unknown"
  helper (Just earlier) =
    let d = truncate $ diffUTCTime now earlier
    in if d == 0 then "new" else
       if d == 1 then "1 second" else
       if d < 60 then show d ++ " seconds" else
       if d < 120 then "1 minute" else
       if d < 3600 then show (d `div` 60) ++ " minutes" else
       if d < 7200 then "1 hour" else
       if d < 86400 then show (d `div` 3600) ++ " hours" else
       if d < 172800 then "1 day" else show (d `div` 86400) ++ " days"
