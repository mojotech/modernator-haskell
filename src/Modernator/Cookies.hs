{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Modernator.Cookies where

import Modernator.Types
import Data.Aeson
import GHC.Generics (Generic)
import Data.Swagger.ParamSchema (ToParamSchema)
import Data.Proxy
import Data.Serialize (Serialize)

data AnswererCookie = AnswererCookie
    { answererId :: AnswererId
    }
    deriving (Generic)

instance ToJSON AnswererCookie
instance FromJSON AnswererCookie
instance ToParamSchema AnswererCookie
instance Serialize AnswererCookie

data QuestionerCookie = QuestionerCookie
    { questionerId :: QuestionerId
    }
    deriving (Generic)

instance ToJSON QuestionerCookie
instance FromJSON QuestionerCookie
instance ToParamSchema QuestionerCookie
instance Serialize QuestionerCookie
