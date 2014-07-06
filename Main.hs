import Network.Wai.Handler.Warp (run)
import Landing.App (app)
import System.Environment (getEnv)

main :: IO ()
main = do
  t <- getEnv "GITHUB_TOKEN"
  let port = 3000
  run port app
