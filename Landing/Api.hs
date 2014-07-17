{-# LANGUAGE OverloadedStrings #-}

module Landing.Api (readme, layout) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (getEnv)
import Landing.Markdown (parseMarkdown)
import Landing.Repo
import qualified Data.ByteString.Lazy as B

readme :: Repo -> IO B.ByteString
readme r@(Repo user repo ref) = do
  token <- getEnv "GITHUB_TOKEN"
  req <- parseUrl $ concat
    [ "https://api.github.com/repos/", user, "/", repo
    , "/readme?access_token=", token
    , fromMaybe [] $ fmap ("&ref="++) ref ]
  let req' = req { requestHeaders =
    [ (hAccept, "application/vnd.github.VERSION.raw")
    , (hUserAgent, "Awesome-Landing-Page-App") ] }
  resp <- withManager $ httpLbs req'
  return . parseMarkdown r $ responseBody resp

layout :: String -> String -> IO B.ByteString
layout user repo = simpleHttp $ concat
  [ "https://raw.githubusercontent.com/"
  , user, "/", repo, "/gh-pages/index.html" ]
