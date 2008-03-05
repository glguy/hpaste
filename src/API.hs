module API (
    module API
  , module Utils.RequestDispatch
  ) where

import Utils.RequestDispatch

mNew  = Method ( (Arg "id"       :: Arg Opt Int)
              :> (Arg "edit"     :: Arg Opt ())
               )
        "GET" "new" "Shows the paste form"

mSave = Method ( (Arg "title"    :: Arg Req String)
              :> (Arg "author"   :: Arg Req String)
              :> (Arg "content"  :: Arg Req String)
              :> (Arg "language" :: Arg Req String)
              :> (Arg "channel"  :: Arg Req String)
              :> (Arg "parent"   :: Arg Opt Int)
              :> (Arg "preview"  :: Arg Opt ())
               )
        "POST" "save" "Saves the paste"

mAddAnnot = Method ( (Arg "id"    :: Arg Req Int)
                  :> (Arg "line"  :: Arg Many Int)
                   )
        "POST" "add_annot" "Annotate a paste"

mDelAnnot = Method ( (Arg "id"    :: Arg Req Int)
                  :> (Arg "line"  :: Arg Many Int)
                   )
        "POST" "del_annot" "Remove annotations from a paste"

mView = Method (Arg "id"      :: Arg Req Int)
        "GET" "view" "Displays a paste"

mRaw  = Method (Arg "id"      :: Arg Req Int)
        "GET" "raw" "Returns paste in text/plain format"

mList = Method ((Arg "search"  :: Arg Opt String)
             :> (Arg "page"    :: Arg Opt Int)
               )
        "GET" "" "List recent pastes"
