{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "index.md" $ do
         route (setExtension "html")
         compile pandocCompiler

    match "index2.md" $ do
         route (setExtension "html")
         compile pandocCompiler2

    match "index3.md" $ do
         route (setExtension "html")
         compile pandocCompiler3

    match "index4.md" $ do
         route (setExtension "html")
         compile pandocCompiler4

pandocCompiler2 = fmap writePandoc (getResourceBody >>= readPandoc)
renderPandoc2 src = fmap writePandoc (readPandoc src)
pandocCompiler3 = getResourceBody >>= renderPandoc
pandocCompiler4 = getResourceBody >>= renderPandoc2
