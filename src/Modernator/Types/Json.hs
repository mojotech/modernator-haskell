{-# LANGUAGE StandaloneDeriving, TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Modernator.Types.Json where

import Modernator.Types ( FullSession
                        , AppError
                        , LockedStatus
                        , Votes(..)
                        , QuestionId(..)
                        , AnswererId(..)
                        , QuestionerId(..)
                        , SessionId(..)
                        , Question(..)
                        , Answered(..)
                        , Answerer(..)
                        , Session(..)
                        , Questioner(..)
                        , SessionMessage(..)
                        )

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.TH as Aeson

-- TH instances
$(Aeson.deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = drop 12 } ''FullSession)

-- Generic instances
instance FromJSON AppError
instance ToJSON AppError

instance ToJSON LockedStatus
instance FromJSON LockedStatus

-- Derived instances
deriving instance ToJSON QuestionId
deriving instance FromJSON QuestionId

deriving instance ToJSON Votes
deriving instance FromJSON Votes

deriving instance ToJSON AnswererId
deriving instance FromJSON AnswererId

deriving instance ToJSON QuestionerId
deriving instance FromJSON QuestionerId

deriving instance ToJSON SessionId
deriving instance FromJSON SessionId

-- Manual instances
instance FromJSON Question where
    parseJSON (Aeson.Object v) =
        Question <$>
            v Aeson..: "questionId" <*>
            v Aeson..: "sessionId" <*>
            v Aeson..: "questionVotes" <*>
            v Aeson..: "questionText" <*>
            v Aeson..: "questionAnswered"
    parseJSON wat = Aeson.typeMismatch "Question" wat
instance ToJSON Question where
    toJSON (Question qId sId votes text answered) =
        object [ "questionId" Aeson..= qId
               , "sessionId" Aeson..= sId
               , "questionVotes" Aeson..= votes
               , "questionText" Aeson..= text
               , "questionAnswered" Aeson..= answered
               ]

instance FromJSON Answered where
    parseJSON (Aeson.Bool True) = pure Answered
    parseJSON (Aeson.Bool False) = pure NotAnswered
    parseJSON wat = Aeson.typeMismatch "Answered" wat
instance ToJSON Answered where
    toJSON Answered = toJSON True
    toJSON NotAnswered = toJSON False

instance FromJSON Answerer where
    parseJSON (Aeson.Object v) =
        Answerer <$>
            v Aeson..: "answererId" <*>
            v Aeson..: "sessionId" <*>
            v Aeson..: "name"
    parseJSON wat = Aeson.typeMismatch "Answerer" wat
instance ToJSON Answerer where
    toJSON (Answerer aId sId name) =
        object ["answererId" Aeson..= aId, "sessionId" Aeson..= sId, "name" Aeson..= name]

instance FromJSON Session where
    parseJSON (Aeson.Object v) =
        Session <$>
            v Aeson..: "sessionId" <*>
            v Aeson..: "name" <*>
            v Aeson..: "expiresAt" <*>
            v Aeson..: "locked"
    parseJSON wat = Aeson.typeMismatch "Session" wat
instance ToJSON Session where
    toJSON (Session sId name expires locked) =
        object
            [ "sessionId" Aeson..= sId
            , "name" Aeson..= name
            , "expiresAt" Aeson..= expires
            , "locked" Aeson..= locked
            ]

instance FromJSON Questioner where
    parseJSON (Aeson.Object v) =
        Questioner <$>
            v Aeson..: "questionerId" <*>
            v Aeson..: "sessionId" <*>
            v Aeson..: "name"
    parseJSON wat = Aeson.typeMismatch "Questioner" wat
instance ToJSON Questioner where
    toJSON (Questioner qId sId name) =
        object ["questionerId" Aeson..= qId, "sessionId" Aeson..= sId, "name" Aeson..= name]

instance ToJSON SessionMessage where
    toJSON SessionLocked = nullaryObject "SessionLocked"
    toJSON SessionExpired = nullaryObject "SessionExpired"
    toJSON SessionClosed = nullaryObject "SessionClosed"
    toJSON (SessionExceptionMessage e) = object [ "tag" Aeson..= (Aeson.String "SessionExceptionMessage"), "exception" Aeson..= e ]
    toJSON (QuestionAsked q) = object [ "tag" Aeson..= (Aeson.String "QuestionAsked"), "question" Aeson..= q ]
    toJSON (QuestionUpvoted q) = object [ "tag" Aeson..= (Aeson.String "QuestionUpvoted"), "question" Aeson..= q ]
    toJSON (QuestionAnswered q) = object [ "tag" Aeson..= (Aeson.String "QuestionAnswered"), "question" Aeson..= q ]
    toJSON (SessionState s) = object [ "tag" Aeson..= (Aeson.String "SessionState"), "session" Aeson..= s ]
instance FromJSON SessionMessage where
    parseJSON (Object o) = do
        tag <- o .: "tag"
        case tag of
            Just (String "SessionLocked") -> pure SessionLocked
            Just (String "SessionExpired") -> pure SessionExpired
            Just (String "SessionClosed") -> pure SessionClosed
            Just (String "SessionExceptionMessage") -> SessionExceptionMessage <$> o .: "exception"
            Just (String "QuestionAsked") -> QuestionAsked <$> o .: "question"
            Just (String "QuestionUpvoted") -> QuestionUpvoted <$> o .: "question"
            Just (String "QuestionAnswered") -> QuestionAnswered <$> o .: "question"
            Just (String "SessionState") -> SessionState <$> o .: "session"
            Just wat -> Aeson.typeMismatch "SessionMessage" wat
            _ -> fail "tag field must be present"
    parseJSON wat = Aeson.typeMismatch "SessionMessage" wat

-- utility functions
nullaryObject tag = object [ "tag" Aeson..= (Aeson.String tag) ]
