{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Modernator.API where

import Servant
import Modernator.SessionsAPI
import Modernator.UsersAPI
import Modernator.WebsocketsAPI
import Modernator.Types
import Modernator.Cookies
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI
import Control.Lens
import Servant.Server.Experimental.Auth.Cookie
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Acid
import Control.Concurrent.STM.TVar (TVar)

type BasicAPI = "sessions" :> (SessionsAPI :<|> WebsocketsAPI)
                :<|> "users" :> UsersAPI

type API = SwaggerSchemaUI "ui" "swagger.js"
           :<|> BasicAPI

api :: Proxy API
api = Proxy

basicAPI :: Proxy BasicAPI
basicAPI = Proxy

server :: RandomSource -> ServerKey -> AuthCookieSettings -> AcidState App -> TVar SessionChannelDB -> Server API
server rng userKey userSettings app sessionChannelDB = swaggerSchemaUIServer swaggerDoc :<|> (sessionsServer app sessionChannelDB :<|> websocketsServer app sessionChannelDB) :<|> usersServer userSession app
    where
        userSession :: (MonadIO m, MonadThrow m) => ModernatorCookie -> User -> m (Headers '[Servant.Header "Set-Cookie" EncryptedSession] User)
        userSession = addSession userSettings rng userKey

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BasicAPI)
    & info.title .~ "Sessions API"
    & info.version .~ "0.1"
    & info.description ?~ "This is an API for creating/joining Q&A sessions and asking questions. Authentication is handled by encrypted session cookies. First create a user account. You will be given a session cookie for the new account after it is successful. You can also login to an existing account to get a session cookie. To perform actions on a specific session you must be a part of it. This is done by either creating the session or joining the session. If you create a session you can answer questions. If you join a session you can ask and upvote questions. Anyone can view the current state of a session at any time."
    & info.license ?~ "MIT"

instance (HasSwagger sub) => HasSwagger (AuthProtect "answerer-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "questioner-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "any-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "user-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))
