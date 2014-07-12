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
  [user, repo, ref] -> landingPage cache user repo (Just ref) >>= f
  [user, repo] -> landingPage cache user repo Nothing >>= f
  [] -> landingPage cache "dennis84" "landing-haskell" Nothing >>= f
  _  -> f notFound

landingPage :: C.Cache -> Text -> Text -> Maybe Text -> IO Response
landingPage cache user repo ref = do
  let cacheKey = C.makeCacheKey user repo ref
  now <- getCurrentTime
  result <- C.lookup cacheKey cache
  html <- case result of
    Just (value, createdAt) -> do
      when (now `diffUTCTime` createdAt > 60) $ C.delete cacheKey cache
      return value
    Nothing -> do
      output <- fetchAndRenderReadme user repo ref 
      _ <- C.insert cacheKey (output, now) cache
      return output
  return $ responseLBS status200
    [(hContentType, "text/html")]
    html

fetchAndRenderReadme :: Text -> Text -> Maybe Text -> IO B.ByteString
fetchAndRenderReadme user repo ref = do
  r <- readme (textToString user) (textToString repo) $ fmap textToString ref
  l <- layout "dennis84" "landing-theme"
  return $ replacePlaceholders
    [("{{USER}}", textToByteString user)
    ,("{{NAME}}", textToByteString repo)
    ,("{{CONTENT}}", r)] l

notFound :: Response
notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
