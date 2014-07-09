{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Time (diffUTCTime, getCurrentTime)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Landing.Cache as C
import Landing.Api
import Landing.Util

app :: C.Cache -> Application
app cache req f = case pathInfo req of
  [u, r] -> repo cache u r >>= f
  [] -> repo cache "dennis84" "landing-haskell" >>= f
  _  -> f notFound

repo :: C.Cache -> Text -> Text -> IO Response
repo cache u r = do
  let cacheKey = C.makeCacheKey u r
  now <- getCurrentTime
  result <- C.lookup cacheKey cache
  html <- case result of
    Just (value, createdAt) -> do
      when (now `diffUTCTime` createdAt > 60) $ C.delete cacheKey cache
      return value
    Nothing -> do
      output <- fetchAndRenderReadme u r
      _ <- C.insert cacheKey (output, now) cache
      return output
  return $ responseLBS status200
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

notFound :: Response
notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
