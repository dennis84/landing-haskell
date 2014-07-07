{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Landing.Api
import Landing.Cache
import Landing.Util
import Data.Time (diffUTCTime, getCurrentTime)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

app :: LRU -> Application
app cache req f = case pathInfo req of
  [u, r] -> repo cache f u r
  [] -> f index
  _  -> f notFound

repo cache f u r = do
  let cacheKey = makeCacheKey u r
  now <- getCurrentTime
  result <- cacheLookup cacheKey cache
  html <- case result of
    Just (v, c) -> do
      when (now `diffUTCTime` c > 60) $ cacheDelete cacheKey cache
      return v
    Nothing -> do
      out <- fetchAndRenderReadme u r
      _ <- cacheInsert cacheKey (out, now) cache
      return out
  f $ responseLBS status200
    [(hContentType, "text/html")]
    html

fetchAndRenderReadme :: Text -> Text -> IO B.ByteString
fetchAndRenderReadme user repo = do
  r <- readme (textToString user) (textToString repo)
  l <- layout "dennis84" "landing-theme"
  return $ replacePlaceholders
    [("{user}", textToByteString user)
    ,("{repo}", textToByteString repo)
    ,("{body}", r)] l

index = responseLBS status200
  [(hContentType, "text/plain")]
  "Hello world!"

notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
