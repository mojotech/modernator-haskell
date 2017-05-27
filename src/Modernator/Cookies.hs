{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Modernator.Cookies where

import Modernator.Types
import Modernator.Types.Json()
import Modernator.Types.Swagger()
import Data.Aeson
import GHC.Generics (Generic)
import Data.Swagger.ParamSchema (ToParamSchema)
import Data.Serialize (Serialize)

data ModernatorCookie = ModernatorCookie
    { userId :: UserId
    }
    deriving (Generic)

instance ToJSON ModernatorCookie
instance FromJSON ModernatorCookie
instance ToParamSchema ModernatorCookie
instance Serialize ModernatorCookie
