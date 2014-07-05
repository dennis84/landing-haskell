{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Landing.Api (readme)
import Landing.Util (textToString)

app :: Application
app req f = case pathInfo req of
  [u, r] -> repo f u r
  [] -> f $ index
  _  -> f $ notFound

repo f u r = do
  resp <- readme (textToString u) (textToString r)
  f $ responseLBS status200
    [(hContentType, "text/plain")]
    resp
  
index = responseLBS status200
  [(hContentType, "text/plain")]
  "Hello world!"

notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
