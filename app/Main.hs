module Main where

import Backend (backend)
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (decodeFileStrict)
import Data.Text.IO (putStrLn)
import Frontend (frontend)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.Run.Files (indexHtml)
import Language.Javascript.JSaddle.WebSockets (jsaddleJs, jsaddleOr)
import Network.HTTP.Types (status200, status405)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort, setTimeout)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.WebSockets (defaultConnectionOptions)
import RIO

main :: IO ()
main = run 8080

app :: JWK -> Application
app jwtKey req sendResponse = case (requestMethod req, pathInfo req) of
  -- If the path starts with "/api", let the backend respond.
  (_, "api" : _) -> backend jwtKey req sendResponse
  -- The jsaddle-warp JavaScript.
  ("GET", ["jsaddle.js"]) ->
    sendResponse $
      responseLBS
        status200
        [("Content-Type", "application/javascript")]
        (jsaddleJs False)
  -- Serve the frontend on any GET request to any path that hasn't been handled.
  ("GET", _) ->
    sendResponse $
      responseLBS
        status200
        [("Content-Type", "text/html")]
        indexHtml
  -- Otherwise, respond with 405.
  _ ->
    sendResponse $
      responseLBS
        status405
        [("Content-Type", "text/plain"), ("Allow", "GET")]
        "Method Not Allowed"

run :: Int -> IO ()
run port = do
  mbJwtKey <- decodeFileStrict "app/jwk.json" -- TODO: USE AN ACTUAL KEY.
  case mbJwtKey of
    Just jwtKey -> do
      let tlsOpts = tlsSettings "app/cert/localhost.crt" "app/cert/localhost.key"
          warpOpts = defaultSettings & setPort port & setTimeout 3600
      putStrLn $ "Running on https://localhost:" <> textDisplay port
      runTLS tlsOpts warpOpts
        =<< jsaddleOr defaultConnectionOptions (frontend >> syncPoint) (app jwtKey)
    Nothing -> error "Did not find a valid JSON web key at `app/jwk.json`!"
