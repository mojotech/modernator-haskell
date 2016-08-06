{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Modernator.API where

import Servant
import Modernator.SessionsAPI
import Data.Proxy
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

data API'
type API = SwaggerUI "ui" SwaggerSchemaEndpoint API'
           :<|> SwaggerSchemaEndpoint
           :<|> "sessions" :> SessionsAPI

instance HasServer API' context where
    type ServerT API' m = ServerT API m
    route _ = route (Proxy :: Proxy API)

type instance IsElem' e API' = IsElem e API

api :: Proxy API
api = Proxy

server a = swaggerUIServer :<|> return swaggerDoc :<|> sessionsServer a

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy SessionsAPI)
