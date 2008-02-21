module Pages where

import Text.XHtml.Strict

import Storage
import Types

stylesheet :: String
stylesheet = "/hpaste.css"

list_page :: [Paste] -> Html
list_page pastes = skin "Recent Pastes" $
  h1 << "Recent Pastes"
 +++
  table ! [theclass "pastelist"]
  << (table_header
  +++ concatHtml
       [tr
        << ((td << show_id p)
        +++ (td << show_author p)
        +++ (td << show_title p)
           )
       | p <- pastes]
     )
  where

  table_header = th << "Paste ID"
             +++ th << "Author"
             +++ th << "Title"

edit_paste_form :: Html
edit_paste_form = skin "New Paste" $
  h1 << "New Paste"
 +++
  form ! [action "save", method "post"]
  << (label ! [thefor "content"]
      << textarea ! [rows "24", cols "80", identifier "content", name "content"]
         << noHtml
  +++ thediv
      << ( -- the nick form
          label ! [thefor "author"]
          << ("author:"
          +++ input ! [ name "author", identifier "author", thetype "text" ]
             )
      +++ label ! [thefor "title"]
           << ("title:" +++ textfield "title")

      +++ input ! [ thetype "image", alt "save" ,theclass "submit"
                  , src "/static/save.jpg", name "submit" ]
          )
     )

display_paste :: Paste -> Html
display_paste paste = skin title_text $
      h1 << show (paste_title paste)
  +++ p << ("Author: " ++ show (paste_author paste))
  +++ p << ("Date: " ++ show (paste_timestamp paste))
  +++ pre ! [theclass "contentbox"] << paste_content paste

  where
  title_text = "Viewing " ++ show_title paste

skin :: String -> Html -> Html
skin title_text body_html =
  header
  << (thetitle << (title_text ++ " - hpaste")
  +++ thelink ! [rel "stylesheet", thetype "text/css", href stylesheet]
      << noHtml
  +++ meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"]
     )

  +++
  body_html
  +++
  thediv ! [theclass "footer"]
  << "hpaste 2008 - haskell-based web devel"

show_id :: Paste -> String
show_id p     = show $ paste_id p

show_author :: Paste -> String
show_author p = maybe "(anonymous)" show $ paste_author p

show_title :: Paste -> String
show_title p  = maybe "(untitled)" show $ paste_title p

