module Pages where

import Text.XHtml.Strict
import Text.Highlighting.Kate

import API
import Utils.URL
import Storage
import Types

stylesheet :: String
stylesheet = "/hpaste.css"

list_page :: [Paste] -> Html
list_page pastes = skin "Recent Pastes" noHtml $
  h2 << "Recent Pastes"
 +++
  table ! [theclass "pastelist"]
  << (table_header
  +++ concatHtml
       [tr
        << (td << anchor ! [href $ exportURL $ methodURL mView $ paste_id p]
                  << "view"
        +++ td << show_author p
        +++ td << show_title p
        +++ td << show_language p
        +++ td << paste_channel p
           )
       | p <- pastes]
     )
  where

  table_header = th << spaceHtml
             +++ th << "Author"
             +++ th << "Title"
             +++ th << "Language"
             +++ th << "Channel"

edit_paste_form :: Maybe Int -> String -> Html
edit_paste_form mb_pasteId starting_text = skin "New Paste" noHtml $
  h2 << "New Paste"
 +++
  form ! [action "save", method "post"]
  << (label ! [thefor "content"]
      << textarea ! [rows "24", cols "80", identifier "content", name "content"]
         << starting_text
  +++ thediv
      << (label ! [thefor "author"]
          << ("author:"
          +++ input ! [ name "author", identifier "author", thetype "text" ]
             )
      +++ label ! [thefor "title"]
           << ("title:" +++ textfield "title")

      +++ input ! [ thetype "image", alt "save" ,theclass "submit"
                  , src "/static/save.jpg", name "submit" ]
          )
  +++ thediv
      << (label ! [thefor "language"]
          << ("language:" +++ language_drop_down)
      +++ label ! [thefor "channel"]
          << ("channel:" +++ channel_drop_down)
         )
  +++ parent_field
      )

  where
  language_drop_down = select ! [name "language", identifier "language"]
                       << (option ! [value ""] << "Plain text"
                       +++ map language_option languages
                          )

  language_option "Haskell" = option ! [selected] << "Haskell"
  language_option l         = option << l

  channel_drop_down = select ! [name "channel", identifier "channel"]
                      << (option ! [selected] << "#haskell"
                      +++ option << emphasize << "none"
                         )

  parent_field = case mb_pasteId of
                   Just pasteId -> hidden "parent" (show pasteId)
                   Nothing      -> noHtml

display_paste :: Paste -> Html
display_paste paste = skin title_text other_links $
      h2 << paste_title paste
  +++ thediv ! [theclass "entrylinks"]
      << (anchor ! [ href $ exportURL
                          $ methodURL mNew (Just (paste_id paste)) (Just ())]
          << "add modification"
      +++ anchor ! [href $ exportURL $ methodURL mRaw (paste_id paste)]
          << "raw"
         )
  +++ style ! [thetype "text/css"]
      << defaultHighlightingCss
  +++ thediv ! [theclass "labels"]
      << (make_label "author" (paste_author paste)
      +++ make_label "date" (paste_timestamp paste)
      +++ make_label "language" (show_language paste)
         )
  +++ thediv ! [theclass "contentbox"] << content

  where
  make_label k v = thespan ! [theclass "labelitem"]
                   << (thespan ! [theclass "labelkey"] << k
                   +++ thespan ! [theclass "labelvalue"] << v)

  title_text = "Viewing " ++ show_title paste

  content
    | null (paste_language paste) = pre ! [theclass "plaintext"]
                                    << paste_content paste
    | otherwise = case highlightAs (paste_language paste)
                                   (paste_content paste) of
                    Left e -> pre << e
                    Right ls -> formatAsXHtml [OptNumberLines]
                                    (paste_language paste) ls

  other_links = anchor ! [href $ exportURL
                          $ methodURL mNew (Just (paste_id paste)) Nothing ]
                << "add revision"

skin :: String -> Html -> Html -> Html
skin title_text other_links body_html =
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
      << (anchor ! [href $ exportURL $ methodURL mList] << "recent"
      +++ anchor ! [href $ exportURL $ methodURL mNew Nothing Nothing] << "new"
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
