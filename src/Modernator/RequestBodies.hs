{-# LANGUAGE DeriveGeneric #-}
module Modernator.RequestBodies where

import Modernator.Types
import Data.Text
import Data.Time.Clock
import Data.Aeson
import GHC.Generics (Generic)
import Data.Swagger.Schema (ToSchema)

data SessionReq = SessionReq
    { sessionName :: Text
    , sessionExpiration :: Maybe UTCTime
    , answererName :: Text
    }
    deriving (Generic, Show)

instance ToJSON SessionReq
instance FromJSON SessionReq
instance ToSchema SessionReq

data JoinReq = JoinReq
    { questionerName :: Maybe Text
    }
    deriving (Generic, Show)

instance ToJSON JoinReq
-- TODO input like { "ques": "foo" } parses correctly as Nothing. It should not.
instance FromJSON JoinReq
instance ToSchema JoinReq

data QuestionReq = QuestionReq
    { question :: Text
    }
    deriving (Generic, Show)

instance ToJSON QuestionReq
instance FromJSON QuestionReq
instance ToSchema QuestionReq
