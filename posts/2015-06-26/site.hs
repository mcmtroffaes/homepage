{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.CSL
import qualified Text.CSL.Output.Pandoc as CSL.Pandoc
import Text.Pandoc

styleCompiler :: Item CSL -> Compiler (Item Style)
styleCompiler csl = do
    style <- unsafeCompiler
             $ readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    makeItem style

readPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item Pandoc)
readPandocBiblioList icsl ibiblio = do
    istyle <- styleCompiler icsl
    let style = itemBody istyle
        Biblio refs = itemBody ibiblio
        formatted = processBibliography procOpts style refs
        blocks = map (return . Plain . CSL.Pandoc.renderPandoc style)
                 $ formatted
        pandoc = Pandoc nullMeta [BulletList blocks]
    makeItem pandoc

renderPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item String)
renderPandocBiblioList csl bib = do
    pd <- readPandocBiblioList csl bib
    return (writePandoc pd)

main = hakyll $ do
    match "style.csl" $ compile cslCompiler
    match "refs.bib" $ compile biblioCompiler
    create ["index.html"] $ do
         route idRoute
         compile $ do
             csl <- load $ fromFilePath "style.csl"
             bib <- load $ fromFilePath "refs.bib"
             html <- renderPandocBiblioList csl bib
             return html
