module Frontend.Utils where

import Reflex
import RIO

holdDynMaybe :: (Reflex t, MonadHold t m) => Event t a -> m (Dynamic t (Maybe a))
holdDynMaybe ev = holdDyn Nothing (Just <$> ev)
