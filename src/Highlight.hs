module Highlight where

import System.Process
import System.IO.UTF8
import System.IO (hClose)

languages = [("Plain","text"),("Haskell","haskell"),("Common Lisp","common-lisp"),("Erlang","erlang"),("Ruby","ruby"),("CSS","css")]

highlightAs lang code
 | lang `notElem` map snd languages = highlightAs "text" code
 | otherwise =
     do (inh, outh, errh, proch) <- runInteractiveProcess
                                                   "/usr/bin/pygmentize"
                                                   ["-f","html","-l",lang,
                                                   "-O","encoding=utf-8"]
                                                   Nothing Nothing
        hPutStr inh code
        hClose inh
        res <- hGetContents outh
        hClose errh
        waitForProcess proch
        return res
