{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll (match "index.html" (route idRoute >> compile getResourceString))
