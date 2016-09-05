{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Modernator.API where

import Servant
import Modernator.SessionsAPI
import Modernator.Types
import Modernator.Cookies
import Data.Proxy
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger.Lens
import Control.Lens
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.ByteString (ByteString)
import GHC.TypeLits (KnownSymbol)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Acid

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type BasicAPI = "sessions" :> SessionsAPI

data API'
type API = SwaggerUI "ui" SwaggerSchemaEndpoint API'
           :<|> SwaggerSchemaEndpoint
           :<|> BasicAPI

type instance IsElem' e API' = IsElem e API

api :: Proxy API
api = Proxy

basicAPI :: Proxy BasicAPI
basicAPI = Proxy

server :: RandomSource -> ServerKey -> ServerKey -> AuthCookieSettings -> AuthCookieSettings -> AcidState App -> Server API
server rng answererKey questionerKey answererSettings questionerSettings app = swaggerUIServer :<|> return swaggerDoc :<|> sessionsServer answererSession questionerSession app
    where
        answererSession :: (MonadIO m, MonadThrow m) => AnswererCookie -> Answerer -> m (Headers '[Servant.Header "Set-Cookie" ByteString] Answerer)
        answererSession = addSession answererSettings rng answererKey
        questionerSession :: (MonadIO m, MonadThrow m) => QuestionerCookie -> Questioner -> m (Headers '[Servant.Header "Set-Cookie" ByteString] Questioner)
        questionerSession = addSession questionerSettings rng questionerKey

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy SessionsAPI)
    & basePath .~ Just "/sessions"
    & info.title .~ "Sessions API"
    & info.version .~ "0.1"
    & info.description ?~ "This is an API for creating/joining Q&A sessions and asking questions"
    & info.license ?~ "MIT"

instance (HasSwagger sub) => HasSwagger (AuthProtect "answerer-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" ByteString :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "questioner-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" ByteString :> sub))

instance (HasSwagger sub) => HasSwagger (AuthProtect "any-auth" :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Cookie" ByteString :> sub))
