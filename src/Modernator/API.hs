{-# LANGUAGE DataKinds, TypeOperators #-}
module Modernator.API where

import Servant
import Modernator.SessionsAPI
import Data.Proxy

type API = "sessions" :> SessionsAPI

api :: Proxy API
api = Proxy

server a = sessionsServer a
