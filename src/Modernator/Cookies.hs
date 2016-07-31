{-# LANGUAGE DeriveGeneric #-}
module Modernator.Cookies where

import Modernator.Types
import Data.Text
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString.Conversion.To (ToByteString, builder)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Servant (FromText, fromText)

data SessionCookie = SessionCookie
    { sessionCookieUser :: Either AnswererId QuestionerId
    }
    deriving (Generic)

instance ToJSON SessionCookie
instance FromJSON SessionCookie
instance ToByteString SessionCookie where
    builder = builder . encode
instance FromText SessionCookie where
    fromText = decode . encodeUtf8 . fromStrict
