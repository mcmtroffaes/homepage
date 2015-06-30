{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.CSL
import Text.CSL.Style hiding (match)
import Text.CSL.Reference
import qualified Text.CSL.Output.Pandoc as CSL.Pandoc
import Text.Pandoc

isArticleJournal :: Reference -> Bool
isArticleJournal ref = refType ref == ArticleJournal

isPaperConference :: Reference -> Bool
isPaperConference ref = refType ref == PaperConference

isTroffaes :: Agent -> Bool
isTroffaes ag = familyName ag == "Troffaes"

article :: Reference -> Bool
article ref = (any isTroffaes (author ref)) && isArticleJournal ref

conf :: Reference -> Bool
conf ref = (any isTroffaes (author ref)) && isPaperConference ref

styleCompiler :: Item CSL -> Compiler (Item Style)
styleCompiler csl = do
    style <- unsafeCompiler
             $ readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    makeItem style

readBiblio :: Item Biblio -> Compiler (Item [Reference])
readBiblio ibiblio = makeItem refs where Biblio refs = itemBody ibiblio

readPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item Pandoc)
readPandocReferences icsl irefs = do
    istyle <- styleCompiler icsl
    let style = itemBody istyle
        refs = itemBody irefs
        formatted = processBibliography procOpts style refs
        blocks = map (return . Plain . CSL.Pandoc.renderPandoc style)
                 $ formatted
        pandoc = Pandoc nullMeta [BulletList blocks]
    makeItem pandoc

renderPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item String)
renderPandocReferences csl refs = do
    pd <- readPandocReferences csl refs
    return (writePandoc pd)

referencesFilterContext :: Item CSL -> String -> (Reference -> Bool) -> Context Biblio
referencesFilterContext csl name condition = field name $ \bib -> do
    refs <- readBiblio bib
    refs2 <- makeItem $ filter condition $ itemBody refs
    html <- renderPandocReferences csl refs2
    return (itemBody html)

referencesContext csl =
    referencesFilterContext csl "conferencepapers" conf `mappend`
    referencesFilterContext csl "journalarticles" article

main = hakyll $ do
    match "style.csl" $ compile cslCompiler
    match "refs.bib" $ compile biblioCompiler
    match "refs.html" $ compile templateCompiler
    create ["index.html"] $ do
         route idRoute
         compile $ do
             csl <- load $ fromFilePath "style.csl"
             bib <- load $ fromFilePath "refs.bib"
             html <- loadAndApplyTemplate "refs.html" (referencesContext csl) bib
             return html
