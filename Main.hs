{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)

main = do
  let port = 3000
  run port app

app :: Application
app req f = case pathInfo req of
  [] -> f $ index
  _  -> f $ notFound

index = responseLBS status200
  [(hContentType, "text/plain")]
  "Hello world!"

notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
