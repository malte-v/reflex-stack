module Frontend
  ( frontend,
  )
where

import Frontend.Router (router)
import Language.Javascript.JSaddle (JSM)
import RIO
import Reflex.Dom hiding (Link, mainWidgetWithCss)
import Reflex.Dom.Main (mainWidgetWithCss)
import Servant.API
import Frontend.API
import Servant.Router hiding (routeURI)
import Servant.Reflex
import Common.API

frontend :: JSM ()
frontend = mainWidgetWithCss "" widget -- TODO clay css

type MyApi = Root :<|> Books :<|> Search

type Root = View

type Books = "books" :> Capture "id" Int :> View

type Search = "search" :> QueryParam "query" Text :> View

myApi :: Proxy MyApi
myApi = Proxy

widget :: forall t m. MonadWidget t m => m ()
widget = do
  let handler :: RouteT MyApi m (Event t Link)
      handler = rootPage :<|> books :<|> search
      books i = do
        el "div" $ text $ "Book: " <> textDisplay i
        pure never
      search Nothing = do
        el "div" $ text "You searched nothing"
        pure never
      search (Just keywords) = do
        el "div" $ text $ "You searched: " <> keywords
        pure never
      fourOhFour = do
        el "p" $ text "404"
        evClickGetName <- button "go to book 42"
        pure $ evClickGetName $> safeLink myApi (Proxy :: Proxy Books) 42
  router myApi handler fourOhFour
  pure ()

rootPage :: forall t m. MonadWidget t m => m (Event t Link)
rootPage = do
  evClickLogin <- button "login"
  evLoginRes <- login (constDyn $ Right $ Login {username = "Ali Baba", password = "Open Sesame"}) evClickLogin <&> fmap \case
    ResponseSuccess _ _ res -> "Status: " <> textDisplay (_xhrResponse_status res)
    ResponseFailure _ msg res -> "Status: " <> textDisplay (_xhrResponse_status res) <> ", msg: " <> msg
    RequestFailure _ msg -> "Request failed: " <> msg
  dynLoginText <- holdDyn "No login response yet :(" evLoginRes
  el "p" $ dynText dynLoginText
  evClickGetName <- button "get the name"
  evGetNameRes <- getName evClickGetName <&> fmap \case
    ResponseSuccess _ name res -> "Status: " <> textDisplay (_xhrResponse_status res) <> ", name: " <> name
    ResponseFailure _ msg res -> "Status: " <> textDisplay (_xhrResponse_status res) <> ", msg: " <> msg
    RequestFailure _ msg -> "Request failed: " <> msg
  dynNumberText <- holdDyn "No name yet :(" evGetNameRes
  el "p" $ dynText dynNumberText
  pure never
