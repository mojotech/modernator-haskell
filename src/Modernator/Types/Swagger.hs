{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Modernator.Types.Swagger where

import Modernator.Types ( FullSession
                        , AppError
                        , LockedStatus
                        , Votes
                        , QuestionId
                        , AnswererId
                        , QuestionerId
                        , SessionId
                        , Question(..)
                        , Answered(..)
                        , Answerer(..)
                        , Session(..)
                        , Questioner(..)
                        , SessionMessage(..)
                        , UserId(..)
                        , User(..)
                        )

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Servant.Server.Experimental.Auth.Cookie (EncryptedSession)

-- | For custom Swagger Schemas
import qualified Data.Aeson.Types as Aeson
import Data.Swagger.Schema hiding (SchemaOptions)
import Data.Swagger.Internal
import Data.Swagger.Lens
import Data.Swagger.ParamSchema (ToParamSchema, toParamSchema, binaryParamSchema)
import Control.Lens hiding (Indexable)
import Data.Proxy

-- Required external instances
instance ToParamSchema EncryptedSession where
    toParamSchema _ = binaryParamSchema

-- Generic instances
instance ToSchema AppError
instance ToSchema QuestionId
instance ToSchema Votes
instance ToSchema AnswererId
instance ToSchema SessionId
instance ToSchema LockedStatus
instance ToSchema QuestionerId
instance ToSchema UserId

instance ToParamSchema QuestionId
instance ToParamSchema AnswererId
instance ToParamSchema SessionId
instance ToParamSchema QuestionerId
instance ToParamSchema UserId

-- Manual Instances
instance ToSchema Question where
    declareNamedSchema _ = do
        questionIdSchema <- declareSchemaRef (Proxy :: Proxy QuestionId)
        sessionIdSchema <- declareSchemaRef (Proxy :: Proxy SessionId)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        votesSchema <- declareSchemaRef (Proxy :: Proxy Votes)
        answeredSchema <- declareSchemaRef (Proxy :: Proxy Answered)
        return $ NamedSchema (Just "Question") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("questionId", questionIdSchema)
                            , ("sessionId", sessionIdSchema)
                            , ("questionVotes", votesSchema)
                            , ("questionText", textSchema)
                            , ("questionAnswered", answeredSchema)
                            ]
            & required .~ ["questionId", "sessionId", "questionVotes", "questionText", "questionAnswered"]

instance ToSchema Answered where
    declareNamedSchema _ = do
        boolSchema <- declareSchema (Proxy :: Proxy Bool)
        return $ NamedSchema (Just "Answered") boolSchema

instance ToSchema Answerer where
    declareNamedSchema _ = do
        answererIdSchema <- declareSchemaRef (Proxy :: Proxy AnswererId)
        sessionIdSchema <- declareSchemaRef (Proxy :: Proxy SessionId)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "Answerer") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("answererId", answererIdSchema)
                            , ("sessionId", sessionIdSchema)
                            , ("name", textSchema)
                            ]
            & required .~ ["answererId", "sessionId", "name"]

instance ToSchema Session where
    declareNamedSchema _ = do
        sessionIdSchema <- declareSchemaRef (Proxy :: Proxy SessionId)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        timeSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        lockedSchema <- declareSchemaRef (Proxy :: Proxy LockedStatus)
        return $ NamedSchema (Just "Session") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("sessionId", sessionIdSchema)
                            , ("name", textSchema)
                            , ("expiresAt", timeSchema)
                            , ("locked", lockedSchema)
                            ]
            & required .~ [ "sessionId", "name", "locked"]

instance ToSchema Questioner where
    declareNamedSchema _ = do
        questionerIdSchema <- declareSchemaRef (Proxy :: Proxy QuestionerId)
        sessionIdSchema <- declareSchemaRef (Proxy :: Proxy SessionId)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        return $ NamedSchema (Just "Questioner") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("questionerId", questionerIdSchema)
                            , ("sessionId", sessionIdSchema)
                            , ("name", textSchema)
                            ]
            & required .~ ["questionerId", "sessionId"]

instance ToSchema FullSession where
    declareNamedSchema _ = do
        sessionSchema <- declareSchemaRef (Proxy :: Proxy Session)
        answererSchema <- declareSchemaRef (Proxy :: Proxy Answerer)
        questionersSchema <- declareSchemaRef (Proxy :: Proxy [Questioner])
        questionsSchema <- declareSchemaRef (Proxy :: Proxy [Question])
        return $ NamedSchema (Just "FullSession") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("session", sessionSchema)
                            , ("answerer", answererSchema)
                            , ("questions", questionsSchema)
                            , ("questioners", questionersSchema)
                            ]
            & required .~ [ "session", "answerer", "questions", "questioners" ]

instance ToSchema SessionMessage where
    declareNamedSchema _ = do
        answererSchema <- declareSchemaRef (Proxy :: Proxy Answerer)
        exceptionSchema <- declareSchemaRef (Proxy :: Proxy AppError)
        questionSchema <- declareSchemaRef (Proxy :: Proxy Question)
        sessionSchema <- declareSchemaRef (Proxy :: Proxy FullSession)
        questionerSchema <- declareSchemaRef (Proxy :: Proxy Questioner)
        let tagSchema = mempty
                & enum_ .~ Just [ Aeson.String "SessionLocked"
                                , Aeson.String "SessionExpired"
                                , Aeson.String "SessionClosed"
                                , Aeson.String "SessionExceptionMessage"
                                , Aeson.String "QuestionAsked"
                                , Aeson.String "QuestionUpvoted"
                                , Aeson.String "QuestionAnswered"
                                , Aeson.String "SessionState"
                                , Aeson.String "QuestionerJoined"
                                ]
                & type_ .~ SwaggerString
        return $ NamedSchema (Just "SessionMessage") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("tag", Inline tagSchema)
                            , ("answerer", answererSchema)
                            , ("exception", exceptionSchema)
                            , ("question", questionSchema)
                            , ("session", sessionSchema)
                            , ("questioner", questionerSchema)
                            ]
            & required .~ ["tag"]
            & description .~ (Just "This is a variant type (sum type, discriminated union) representing the possible session messages. The `tag` field determines which fields are additionally present. Unless otherwise specified, only the `tag` field is present. If `tag` is `SessionStarted`, `answerer` is present. If `tag` is `SessionExceptionMessage`, `exception` is present. If `tag` is `QuestionAsked`, `QuestionUpvoted` or `QuestionAnswered`, `question` is present. If `tag` is `SessionState`, `session` is present. If `tag` is `QuestionerJoined`, `questioner` is present.")

instance ToSchema User where
    declareNamedSchema _ = do
        userIdSchema <- declareSchemaRef (Proxy :: Proxy UserId)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        asSchema <- declareSchemaRef (Proxy :: Proxy [(SessionId, AnswererId)])
        qsSchema <- declareSchemaRef (Proxy :: Proxy [(SessionId, QuestionerId)])
        return $ NamedSchema (Just "User") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ [ ("userId", userIdSchema)
                            , ("userName", textSchema)
                            , ("answererSessions", asSchema)
                            , ("questionerSessions", qsSchema)
                            ]
            & required .~ [ "userId", "userName", "answererSessions", "questionerSessions" ]
