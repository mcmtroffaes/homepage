{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "index.html" $ do
         route idRoute
         compile copyFileCompiler
