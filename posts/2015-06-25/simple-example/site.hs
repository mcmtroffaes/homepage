{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    -- first example
    match "*1.md" $ do
         route (setExtension "html")
         compile pandocCompiler

    -- second example
    match "default.html" $ compile templateCompiler
    match "default.css" $ do
        route idRoute
        compile copyFileCompiler
    match "*2.md" $ do
         route (setExtension "html")
         compile $ pandocCompiler
             >>= loadAndApplyTemplate "default.html" defaultContext
             >>= relativizeUrls

   -- third example
    match "*3.md" $ do
         route (setExtension "html")
         compile $ do
             html1 <- pandocCompiler
             html2 <- loadAndApplyTemplate "default.html" defaultContext html1
             html3 <- relativizeUrls html2
             return html3
