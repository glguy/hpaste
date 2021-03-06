{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pages where

import Text.XHtml.Strict hiding (URL)
import Data.Maybe (isNothing)
import Data.Time
import MonadLib

import API
import Config
import Types
import Utils.URL

data PageEnv = PageEnv { page_env_conf    :: Config
                       , page_env_baseurl :: String }

newtype PageM a = PageM (ReaderT PageEnv Id a)
 deriving (Monad, Applicative, Functor)

runPageM conf baseurl (PageM m) = runId $ runReaderT (PageEnv conf baseurl) m

pages f = PageM (asks f)

make_url :: URL -> PageM String
make_url url = do b <- pages page_env_baseurl
                  return $ b ++ exportURL url

error_page :: [Html] -> PageM Html
error_page errors =
  skin "Validation problem" noHtml noHtml $
  h2 << "Paste validation failed"
  +++
  ordList errors
  +++
  p << "Please use your back button to correct your paste."

list_page :: UTCTime -> [Paste] -> Maybe String -> Int -> PageM Html
list_page now pastes pat offset = do
  urls        <- mapM (make_url . methodURL mView . paste_id) pastes
  n           <- pages (pastes_per_page . page_env_conf)
  earlier_url <- make_url (methodURL mList pat (Just (offset + 1)))
  later_url   <- make_url (methodURL mList pat (Just (offset - 1)))
  skin the_title noHtml noHtml $
      the_heading
      +++ html_result urls n earlier_url later_url
      +++ search_form

  where
  the_title = case pat of
                Just query -> "Search results for " ++ query
                Nothing    -> "Recent Pastes"

  the_heading = h2
                << case pat of
                     Just query -> "Search results for " +++ emphasize << query
                     Nothing    -> noHtml

  search_form = form ! [method "get"]
                << (textfield "search"
                +++ submit "search" "search")

  html_result urls n earlier_url later_url =
     table ! [theclass "pastelist"]
     << (table_header
     +++ concatHtml
          [tr
           << (td << show_author p
           +++ td << anchor ! [href view_url] << show_title p
           +++ td << show_ago now p
           +++ td << paste_language p
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

edit_paste_form :: Maybe Int -> String -> String
                -> [(String,String)] -> PageM Html
edit_paste_form mb_pasteId language starting_text langs =
  skin page_title noHtml noHtml $
  h2 ! [theclass "newheader"] << page_title
 +++
  form ! [action "save", method "post"]
  << (textarea ! [rows "24", cols "80",identifier "content",name "content"]
      << starting_text
  +++ thediv ! [theclass "tabsrow1"]
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
                  , name "save", value "save" ]
      +++ input ! [ thetype "submit", alt "save" ,theclass "imagebutton"
                  , name "preview", value "preview" ]
          )
  +++ thediv ! [theclass "tabsrow2"]
      << (label ! [thefor "language"]
          << (thespan << "language " +++ language_drop_down)
      +++ label ! [thefor "channel"]
          << (thespan << "channel " +++ channel_drop_down)
         )
  +++ parent_field
     )

  where
  page_title = case mb_pasteId of Just _ -> "New Revision"
                                  Nothing -> "New Paste"

  language_drop_down = select ! [name "language", identifier "language"]
                       << map language_option langs

  language_option (k,v) | v == language = option ! [selected, value v] << k
                        | otherwise     = option ! [value v] << k

  channel_drop_down = select ! [name "channel", identifier "channel"]
                      << (option << emphasize << "none"
                         )

  parent_field = case mb_pasteId of
                   Just pasteId -> hidden "parent" (show pasteId)
                   Nothing      -> noHtml

display_preview :: Paste -> String -> PageM Html
display_preview paste htm =
 do content <- display_paste (error "whoops") Nothing (paste,htm)
    skin the_title noHtml noHtml $
     p << "This is a preview"
     +++ content
     +++ save_form
  where
  the_title = "Previewing " ++ show_title paste
  save_form = form ! [method "POST", action "save"]
              << (hidden "author" (paste_author paste)
              +++ hidden "title" (paste_title paste)
              +++ hidden "content" (paste_content paste)
              +++ hidden "language" (paste_language paste)
              +++ hidden "channel" (paste_channel paste)
              +++ (case paste_parentid paste of
                      Nothing -> noHtml
                      Just p -> hidden "parent" (show p))
              +++ input ! [ thetype "submit", alt "save"
                          , theclass "imagebutton" , name "save"
                          , value "save" ])

display_pastes :: UTCTime -> Paste -> [Paste] -> [String] -> PageM Html
display_pastes now x xs cs =
 do new_url  <- make_url (methodURL mNew (Just (paste_id x)) Nothing)
    view_url <- make_url (methodURL mView (paste_id x))
    css_url  <- make_url (methodURL mAnnotCss (paste_id x))
    content  <- mapM (display_paste now (Just view_url))
                         (zip (x:xs) cs)
    skin the_title (other_links new_url) (css_link css_url) (toHtml content)
  where
  the_title = "Viewing " ++ show_title x
  other_links new_url = anchor ! [href new_url] << "add revision"


display_paste :: UTCTime -> Maybe String -> (Paste, String) -> PageM Html
display_paste now mb_view_url (paste, rendered) =
  make_url (methodURL mNew (Just (paste_id paste)) (Just ())) >>= \ new_url ->
  make_url (methodURL mRaw (paste_id paste)) >>= \ raw_url ->
  return $
     (case mb_view_url of
        Nothing -> noHtml
        Just view_url ->
          thediv ! [theclass "entrylinks"]
          << (anchor ! [ href new_url] << "modify"
          +++ " "
          +++ anchor ! [href raw_url] << "download"
          +++ " "
          +++ anchor ! [identifier ("a" ++ show (paste_id paste)),
                        href (view_url ++ "#a" ++ show(paste_id paste)) ]
                        << "link"
         ))
  +++ thediv ! [theclass "pasteheader"]
      << (h2 << show_title paste
      +++ thediv ! [theclass "labels"]
          << (defList $ zip ["author","age","language"]
                            [ show_author paste
                            , show_ago now paste
                            , paste_language paste]
             )
      +++ thediv ! [theclass "clearer"] << noHtml
         )
  +++ thediv ! [theclass "contentbox"] << primHtml rendered
  +++
  (if isNothing mb_view_url then noHtml else
      form ! [method "POST", action "add_annot"]
      << (hidden "id" (show (paste_id paste))
      +++ textfield "line.0"
      +++ submit "add" "Add highlight")
  +++ form ! [method "POST", action "del_annot"]
      << (hidden "id" (show (paste_id paste))
      +++ textfield "line.0"
      +++ submit "submit" "Remove highlight")
  )

skin :: String -> Html -> Html -> Html -> PageM Html
skin title_text other_links head_html body_html =
 do stylesheets <- pages (style_path . page_env_conf)
    list_url    <- make_url (methodURL mList Nothing Nothing)
    new_url     <- make_url (methodURL mNew Nothing Nothing)
    return $
     header
     << (thetitle << (title_text ++ " - hpaste")
     +++ map css_link stylesheets
     +++ meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"]
     +++ head_html)
     +++
     body
     << (h1 << anchor ! [href list_url] << "hpastetwo"
     +++ thediv ! [theclass "toplinks"]
         << (anchor ! [href list_url] << "recent"
         +++ " "
         +++ anchor ! [href new_url] << "new"
         +++ " "
         +++ other_links)
     +++ thediv ! [theclass "wrapper"] << body_html
     +++ thediv ! [theclass "footer"]
         << "hpaste 2008 - git clone http://code.haskell.org/hpaste.git")

show_author :: Paste -> String
show_author p | paste_author p == "" = "(anonymous)"
              | otherwise            = paste_author p

show_title :: Paste -> String
show_title p | paste_title p == "" = "(untitled)"
             | otherwise           = paste_title p

show_ago :: UTCTime -> Paste -> String
show_ago now paste = helper (paste_timestamp paste)
  where
  helper Nothing = "new"
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

css_link url = thelink ! [rel "stylesheet", thetype "text/css", href url]
               << noHtml
