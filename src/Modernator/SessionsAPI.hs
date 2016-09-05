{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies #-}
module Modernator.SessionsAPI where

import Modernator.Types
import Modernator.Commands
import Modernator.APIUtils
import Modernator.RequestBodies
import Modernator.Cookies
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.Proxy
import Data.Acid
import Data.Time.Clock
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Swagger.ParamSchema (ToParamSchema, toParamSchema)

type instance AuthServerData (AuthProtect "answerer-auth") = AnswererCookie
type instance AuthServerData (AuthProtect "questioner-auth") = QuestionerCookie

type SessionsAPI =
    ReqBody '[JSON] SessionReq :> Post '[JSON] (Headers '[Header "Set-Cookie" ByteString] Answerer)
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "lock" :> PostNoContent '[PlainText] NoContent
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> DeleteNoContent '[PlainText] NoContent
    :<|> ReqBody '[JSON] JoinReq :> Capture "session_id" SessionId :> "join" :> Post '[JSON] (Headers '[Header "Set-Cookie" ByteString] Questioner)
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> "ask" :> ReqBody '[JSON] QuestionReq :> Post '[JSON] Question
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "upvote" :> Post '[JSON] Question
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "answer" :> Post '[JSON] Question

sessionsAPI :: Proxy SessionsAPI
sessionsAPI = Proxy

sessionsServer ::
    (AnswererCookie -> Answerer -> Handler (Headers '[Header "Set-Cookie" ByteString] Answerer)) ->
    (QuestionerCookie -> Questioner -> Handler (Headers '[Header "Set-Cookie" ByteString] Questioner)) ->
    AcidState App -> Server SessionsAPI
sessionsServer addAnswererSession addQuestionerSession app = newSessionH :<|> lockSessionH :<|> deleteSessionH :<|> joinSessionH :<|> askQH :<|> upvoteQH :<|> answerQH
    where
        newSessionH req = do
            answerer@(Answerer id _ _) <- liftIO . newSessionHandler app $ req
            addAnswererSession (AnswererCookie id) answerer
        lockSessionH c id = withError <=< fmap (fmap unitToNoContent) . liftIO . sessionLockHandler app c $ id
        deleteSessionH c id = withError <=< fmap (fmap unitToNoContent) . liftIO . sessionDeleteHandler app c $ id
        joinSessionH req sessionId = do
            questioner@(Questioner id _ _) <- withError <=< liftIO . sessionJoinHandler app req $ sessionId
            addQuestionerSession (QuestionerCookie id) questioner
        askQH cookie sessionId = withError <=< liftIO . askQuestionHandler app cookie sessionId
        upvoteQH cookie sessionId = withError <=< liftIO . upvoteQuestionHandler app cookie sessionId
        answerQH cookie sessionId = withError <=< fmap (fmap unitToNoContent) . liftIO . answerQuestionHandler app cookie sessionId

newSessionHandler :: AcidState App -> SessionReq -> IO Answerer
newSessionHandler app (SessionReq name expiration answererName) = update app (NewSession name expiration answererName)

sessionLockHandler :: AcidState App -> AnswererCookie -> SessionId -> IO (Either AppError ())
sessionLockHandler app (AnswererCookie id) sessionId = update app (LockSession id sessionId)

sessionDeleteHandler :: AcidState App -> AnswererCookie -> SessionId -> IO (Either AppError ())
sessionDeleteHandler app (AnswererCookie id) sessionId = update app (DeleteSession id sessionId)

sessionJoinHandler :: AcidState App -> JoinReq -> SessionId -> IO (Either AppError Questioner)
sessionJoinHandler app (JoinReq name) sessionId = update app (JoinSession sessionId name)

askQuestionHandler :: AcidState App -> QuestionerCookie -> SessionId -> QuestionReq -> IO (Either AppError Question)
askQuestionHandler app (QuestionerCookie questionerId) sessionId (QuestionReq question) = update app (AddQuestion question sessionId questionerId)

upvoteQuestionHandler :: AcidState App -> QuestionerCookie -> SessionId -> QuestionId -> IO (Either AppError Question)
upvoteQuestionHandler app (QuestionerCookie questionerId) sessionId questionId = update app (UpvoteQuestion questionId sessionId questionerId)

answerQuestionHandler :: AcidState App -> AnswererCookie -> SessionId -> QuestionId -> IO (Either AppError Question)
answerQuestionHandler app (AnswererCookie  answererId) sessionId questionId = update app (AnswerQuestion questionId sessionId answererId)

instance ToParamSchema ByteString where
    toParamSchema _ = toParamSchema (Proxy :: Proxy String)
