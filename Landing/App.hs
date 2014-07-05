{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Data.ByteString.Lazy.Char8 ()
import Landing.Api (readme)

app :: Application
app req f = case pathInfo req of
  [u, r] -> f $ repo u r
  [] -> f $ index
  _  -> f $ notFound

repo u r = responseLBS status200
  [(hContentType, "text/plain")]
  "Repo!"
  
index = responseLBS status200
  [(hContentType, "text/plain")]
  "Hello world!"

notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
