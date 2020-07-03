{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Frontend.API where

import Common.API
import JSDOM (currentDocument)
import qualified JSDOM.Document as Doc
import JSDOM.Types (JSM)
import RIO hiding ((.~))
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as TextP
import Reflex.Dom
import Servant.API
import Servant.Auth.Server (SetCookie)
import Servant.Reflex
import Control.Lens

getName ::
  forall t m tag.
  MonadWidget t m =>
  Event t tag ->
  m (Event t (ReqResult tag Text))

getEmail ::
  forall t m tag.
  MonadWidget t m =>
  Event t tag ->
  m (Event t (ReqResult tag Text))

login ::
  forall t m tag.
  MonadWidget t m =>
  Dynamic t (Either Text Login) ->
  Event t tag ->
  m (Event t (ReqResult tag (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)))

-- | Generate the client functions
( ( getName :<|>
      getEmail
    ) :<|>
    login
  ) =
    clientWithOpts
      (Proxy :: Proxy API)
      (Proxy :: Proxy m)
      (Proxy :: Proxy tag)
      (constDyn $ BaseFullUrl Https "localhost" 8080 "/")
      clientOpts

clientOpts :: ClientOptions
clientOpts = ClientOptions tweakReq
  where
    tweakReq r = do
      mbCookie <- findCookie "XSRF-TOKEN"
      pure $ r & headerMod "X-XSRF-TOKEN" .~ mbCookie
    headerMod d = xhrRequest_config . xhrRequestConfig_headers . at d

getCookies :: JSM (Maybe Text)
getCookies =
  currentDocument
    >>= maybe (pure Nothing) (fmap Just . Doc.getCookie)

-- | find the cookie from the cookie string
findCookie :: Text.Text -> JSM (Maybe Text.Text)
findCookie which = do
  mayCookies <- getCookies
  case mayCookies of
    Nothing -> pure Nothing
    Just cookies -> pure $ Just $ Text.takeWhile (/= ';') $ Text.drop 1 $ Text.dropWhile (/= '=') $ snd $ TextP.breakOn which cookies
