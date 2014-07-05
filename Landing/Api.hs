module Landing.Api (readme) where

import Network.HTTP
import Network.Stream
import System.Environment (getEnv)

readme :: String -> String -> IO (Either ConnError (Response String))
readme user repo = do
  token <- getEnv "GITHUB_TOKEN"
  res <- simpleHTTP $ getRequest $
    "https://api.github.com/repos/" ++ user ++ "/" ++ repo ++ 
    "/readme?access_token=" ++ token
  return res
