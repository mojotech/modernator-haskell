{-# LANGUAGE DeriveGeneric #-}
module Modernator.RequestBodies where

import Modernator.Types
import Data.Text
import Data.Time.Clock
import Data.Aeson
import GHC.Generics (Generic)

data SessionReq = SessionReq
    { sessionName :: Text
    , sessionExpiration :: Maybe UTCTime
    , answererName :: Text
    }
    deriving (Generic)

instance FromJSON SessionReq

data JoinReq = JoinReq
    { questionerName :: Maybe Text
    }
    deriving (Generic)

-- TODO input like { "ques": "foo" } parses correctly as Nothing. It should not.
instance FromJSON JoinReq

data QuestionReq = QuestionReq
    { question :: Text
    }
    deriving (Generic)

instance FromJSON QuestionReq
