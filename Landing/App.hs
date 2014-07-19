{-# LANGUAGE OverloadedStrings #-}

module Landing.App (app) where

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Time (diffUTCTime, getCurrentTime)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Landing.Cache as C
import Landing.Api (readme, layout)
import Landing.Util (replacePlaceholders)
import Landing.Repo

app :: C.Cache -> Application
app cache req respond = case pathInfo req of
  [user, repo, ref] -> respond =<< landingPage cache (fromText user repo $ Just ref)
  [user, repo]      -> respond =<< landingPage cache (fromText user repo Nothing)
  []                -> respond =<< landingPage cache (fromText "dennis84" "landing-haskell" Nothing)
  _                 -> respond notFound

landingPage :: C.Cache -> Repo -> IO Response
landingPage cache repo = do
  let cacheKey = joinPath repo
  now <- getCurrentTime
  result <- C.lookup cacheKey cache
  html <- case result of
    Just (value, createdAt) -> do
      when (now `diffUTCTime` createdAt > 60) $ C.delete cacheKey cache
      return value
    Nothing -> do
      output <- fetchAndRenderReadme repo
      _ <- C.insert cacheKey (output, now) cache
      return output
  return $ responseLBS status200
    [(hContentType, "text/html")]
    html

fetchAndRenderReadme :: Repo -> IO B.ByteString
fetchAndRenderReadme r@(Repo user repo ref) = do
  c <- readme r
  l <- layout "dennis84" "landing-theme"
  return $ replacePlaceholders
    [("{{USER}}", C.pack user)
    ,("{{NAME}}", C.pack repo)
    ,("{{CONTENT}}", c)] l

notFound :: Response
notFound = responseLBS status404
  [(hContentType, "text/plain")]
  "Not found!"
