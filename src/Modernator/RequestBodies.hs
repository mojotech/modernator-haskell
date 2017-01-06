{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLists #-}
module Modernator.RequestBodies where

import Modernator.Types()
import Data.Text
import Data.Time.Clock
import Data.Aeson
import GHC.Generics (Generic)
import Data.Swagger.Schema
import Control.Lens
import Data.Swagger.Internal
import Data.Swagger.Lens
import Data.Proxy

data SessionReq = SessionReq
    { sessionName :: Text
    , sessionExpiration :: Maybe UTCTime
    , answererName :: Text
    }
    deriving (Generic, Show, Eq)

instance ToJSON SessionReq
instance FromJSON SessionReq
instance ToSchema SessionReq

data JoinReq = JoinReq
    { questionerName :: Maybe Text
    }
    deriving (Generic, Show, Eq)

instance ToJSON JoinReq
-- TODO input like { "ques": "foo" } parses correctly as Nothing. It should not.
instance FromJSON JoinReq
instance ToSchema JoinReq where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "JoinReq") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [("questionerName", textSchema)]

data QuestionReq = QuestionReq
    { question :: Text
    }
    deriving (Generic, Show, Eq)

instance ToJSON QuestionReq
instance FromJSON QuestionReq
instance ToSchema QuestionReq
