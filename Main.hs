import Network.Wai.Handler.Warp (run)
import Landing.App (app)
import System.Environment (getEnv)
import Control.Exception

getPort :: IO Int
getPort = (getEnv "PORT" >>= return . read) `catch` (\(SomeException _) -> return 3000)

main :: IO ()
main = do
  _ <- getEnv "GITHUB_TOKEN"
  p <- getPort
  putStrLn $ "Listening on port " ++ show p
  run p app
