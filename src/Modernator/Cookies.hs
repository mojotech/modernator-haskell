{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Modernator.Cookies where

import Modernator.Types
import Data.Text
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString.Conversion.To (ToByteString, builder)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Data.Swagger.ParamSchema (ToParamSchema, toParamSchema)
import Data.Proxy
import Web.HttpApiData (FromHttpApiData, parseQueryParam)

data SessionCookie = SessionCookie
    { sessionCookieUser :: Either AnswererId QuestionerId
    }
    deriving (Generic)

instance ToJSON SessionCookie
instance FromJSON SessionCookie
instance ToByteString SessionCookie where
    builder = builder . encode
instance ToParamSchema SessionCookie where
    toParamSchema _ = toParamSchema (Proxy :: Proxy (Either AnswererId QuestionerId))
instance FromHttpApiData SessionCookie where
    parseQueryParam = fmap SessionCookie . parseQueryParam

instance ToParamSchema (Either AnswererId QuestionerId) where
    toParamSchema _ = toParamSchema (Proxy :: Proxy Integer)
