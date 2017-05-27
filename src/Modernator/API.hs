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

type API = SwaggerSchemaUI "swagger.js" "ui"
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
    & info.description ?~ "This is an API for creating/joining Q&A sessions and asking questions. Authentication is handled by encrypted session cookies. When you create a session you are automatically authed as an answerer for that session. When you join a session you are automatically authed as a questioner for that session. You can only be authed to one session at a time, and creating new sessions will revoke your authentication for previous sessions. The authorization for a questioner and an answerer are two different cookies. The questioner cookie contains a JSON representation of a QuestionerId. The answerer cookie contains a JSON representation of an AnswererId."
    & info.license ?~ "MIT"

instance (HasSwagger sub) => HasSwagger (AuthProtect "answerer-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "questioner-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "any-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "user-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" EncryptedSession :> sub))
