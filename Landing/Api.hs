{-# LANGUAGE OverloadedStrings #-}

module Landing.Api (readme, layout) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Types
import System.Environment (getEnv)
import Landing.Markdown (parseMarkdown)
import Landing.Repo (Repo(..))
import qualified Data.ByteString.Lazy as B

readme :: Repo -> IO B.ByteString
readme r@(Repo user repo ref) = do
  token <- getEnv "GITHUB_TOKEN"
  req <- parseUrl $ concat
    [ "https://api.github.com/repos/", user, "/", repo
    , "/readme?access_token=", token
    , fromMaybe [] $ (++) "&ref=" <$> ref ]
  let req' = req { requestHeaders =
    [ (hAccept, "application/vnd.github.VERSION.raw")
    , (hUserAgent, "Awesome-Landing-Page-App") ] }
  man <- newManager defaultManagerSettings
  resp <- httpLbs req' man
  return . parseMarkdown r $ responseBody resp

layout :: String -> String -> IO B.ByteString
layout user repo = do
  req <- parseUrl $ concat
    [ "https://raw.githubusercontent.com/"
    , user, "/", repo, "/gh-pages/index.html" ]
  man <- newManager defaultManagerSettings
  res <- httpLbs req man
  return $ responseBody res
