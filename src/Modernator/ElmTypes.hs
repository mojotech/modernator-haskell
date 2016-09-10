{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, TypeOperators, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Modernator.ElmTypes where

import Modernator.Types
import Modernator.RequestBodies
import Elm (ElmType, toElmType, ElmTypeExpr)
import Servant.Foreign (HasForeign, foreignFor, Foreign)
import Servant.API (Headers, AuthProtect, (:>))
import Data.Proxy

instance ElmType AppError
instance ElmType SessionId
instance ElmType SessionReq
instance ElmType JoinReq
instance ElmType Questioner
instance ElmType QuestionerId
instance ElmType Answerer
instance ElmType AnswererId

instance (ElmType a) => ElmType (Headers unused a) where
    toElmType _ = toElmType (Proxy :: Proxy a)

instance (HasForeign e f rest) => HasForeign e f (AuthProtect sym :> rest) where
    type Foreign f (AuthProtect sym :> rest) = Foreign f rest

    foreignFor lang f _ = foreignFor lang f (Proxy :: Proxy rest)
