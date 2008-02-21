module API (
    module API
  , module Utils.RequestDispatch
  ) where

import Utils.RequestDispatch

mNew  = Method () "GET" "new" "Shows the paste form"

mSave = Method ( (Arg "title"   :: Arg Req String)
              :> (Arg "author"  :: Arg Req String)
              :> (Arg "content" :: Arg Req String)
              :> (Arg "save"    :: Arg Opt ())
              :> (Arg "preview" :: Arg Opt ())
               )
        "POST" "save" "Saves the paste"

mView = Method (Arg "id"      :: Arg Req Int)
        "GET" "view" "Displays a paste"

mList = Method () "GET" "" "List recent pastes"
