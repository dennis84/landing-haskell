{-# LANGUAGE OverloadedStrings #-}

module Landing.Api (readme) where

import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (getEnv)

readme user repo = do
  token <- getEnv "GITHUB_TOKEN"
  req <- parseUrl ("https://api.github.com/repos/" ++
                   user ++ "/" ++ repo ++ 
                   "/readme?access_token=" ++ token)
  let req' = req { requestHeaders = [(hAccept,    "application/vnd.github.VERSION.raw")
                                   , (hUserAgent, "Awesome-Landing-Page-App")] }
  resp <- withManager $ httpLbs req'
  return $ responseBody resp
