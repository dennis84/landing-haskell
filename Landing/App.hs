{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Landing.Api (readme, layout)
import Landing.Util (textToString, textToByteString, replacePlaceholders)

app :: Application
app req f = case pathInfo req of
  [u, r] -> repo f u r
  [] -> f $ index
  _  -> f $ notFound

repo f u r = do
  b <- readme (textToString u) (textToString r)
  l <- layout "dennis84" "landing-theme"
  let html = replacePlaceholders
               [("{user}", textToByteString u)
               ,("{repo}", textToByteString r)
               ,("{body}", b)] l
  f $ responseLBS status200
    [(hContentType, "text/html")]
    html
  
index = responseLBS status200
  [(hContentType, "text/plain")]
  "Hello world!"

notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
