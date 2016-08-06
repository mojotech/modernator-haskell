{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls #-}
module Modernator.API where

import Servant
import Modernator.SessionsAPI
import Data.Proxy
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type BasicAPI = "sessions" :> SessionsAPI

data API'
type API = SwaggerUI "ui" SwaggerSchemaEndpoint API'
           :<|> SwaggerSchemaEndpoint
           :<|> BasicAPI

instance HasServer API' context where
    type ServerT API' m = ServerT API m
    route _ = route (Proxy :: Proxy API)

type instance IsElem' e API' = IsElem e API

api :: Proxy API
api = Proxy

basicAPI :: Proxy BasicAPI
basicAPI = Proxy

server a = swaggerUIServer :<|> return swaggerDoc :<|> sessionsServer a

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy SessionsAPI)
