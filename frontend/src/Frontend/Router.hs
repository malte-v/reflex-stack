module Frontend.Router
  ( router,
  )
where

import qualified Data.ByteString.UTF8 as BSU
import JSDOM.Types (SerializedScriptValue (..), liftJSM, toJSVal)
import Language.Javascript.JSaddle.Value (JSNull (..))
import Network.HTTP.Types.URI
import RIO
import Reflex.Dom hiding (mainWidgetWithCss, Link)
import Servant.API
import Servant.Links (linkURI)
import Servant.Router hiding (routeURI)

router ::
  forall t m layout.
  (MonadWidget t m, HasRouter layout) =>
  Proxy layout ->
  RouteT layout m (Event t Link) ->
  m (Event t Link) ->
  m ()
router layoutProxy handler fourOhFour = do
  let uriRouter = routeURI layoutProxy handler
  rec dynHistoryItem <- manageHistory evHistoryCommand
      let dynUri = _historyItem_uri <$> dynHistoryItem
      evEvNavigateOrErr <- dyn (uriRouter <$> dynUri)
      let evMEvNavigate = ffor
            evEvNavigateOrErr
            \case
              Right ev -> pure ev
              Left _ -> fourOhFour
      dynMEvNavigate <- holdDyn (pure never) evMEvNavigate
      evEvNavigate <- dyn dynMEvNavigate
      evNavigate <- switchHold never evEvNavigate
      jsNull <- liftJSM $ toJSVal JSNull
      let evHistoryCommand = ffor evNavigate \routeLink ->
            HistoryCommand_PushState $
              HistoryStateUpdate
                { _historyStateUpdate_uri = Just (linkURI routeLink),
                  _historyStateUpdate_title = "some title", -- TODO this doesn't work
                  _historyStateUpdate_state = SerializedScriptValue jsNull
                }
  pure ()

routeURI ::
  forall m a layout.
  (HasRouter layout, Monad m) =>
  Proxy layout ->
  RouteT layout m a ->
  URI ->
  m (Either RoutingError a)
routeURI layout page uri =
  let servantRouter = route layout Proxy Proxy page
      queryParams = parseQuery . BSU.fromString $ uriQuery uri
      pathSegments = decodePathSegments . BSU.fromString $ uriPath uri
   in routeQueryAndPath queryParams pathSegments servantRouter
