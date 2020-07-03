module Backend (backend) where

import Common.API
import RIO hiding (Handler, words)
import Servant
import Servant.Auth.Server
import Crypto.JOSE.JWK (JWK)

protected :: AuthResult User -> Server Protected
protected (Authenticated user) = pure (name user) :<|> pure (email user)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cookieSettings jwtSettings = checkCreds cookieSettings jwtSettings

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Handler
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
  -- Usually you would ask a database for the user info. This is just a
  -- regular servant handler, so you can follow your normal database access
  -- patterns (including using 'enter').
  let usr = User "Ali Baba" "ali@email.com"
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> pure $ applyCookies NoContent
checkCreds _ _ _ = throwError err401

server :: CookieSettings -> JWTSettings -> Server API
server cookieSettings jwtSettings = protected :<|> unprotected cookieSettings jwtSettings

backend :: JWK -> Application
backend jwtKey = do
  let jwtCfg = defaultJWTSettings jwtKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext -- TODO: Enable XSRF protection!
      api = Proxy :: Proxy API
  serveWithContext api cfg (server defaultCookieSettings jwtCfg)
