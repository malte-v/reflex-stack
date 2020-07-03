module Common.API where

import Data.Aeson (FromJSON, ToJSON)
import RIO
import Servant.API
import Servant.Auth (Auth, Cookie)
import Servant.Auth.Server (FromJWT, SetCookie, ToJWT)

data User = User {name :: Text, email :: Text}
  deriving (Eq, Show, Read, Generic)

instance ToJSON User

instance FromJSON User

instance ToJWT User

instance FromJWT User

data Login = Login {username :: Text, password :: Text}
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login

instance FromJSON Login

type Protected =
  "name" :> Get '[JSON] Text
    :<|> "email" :> Get '[JSON] Text

type Unprotected =
  "login"
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[PlainText]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             NoContent
         )

type API = "api" :> ((Auth '[Cookie] User :> Protected) :<|> Unprotected)
