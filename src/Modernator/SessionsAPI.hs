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
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Swagger.ParamSchema (ToParamSchema, toParamSchema)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (newBroadcastTChan, writeTChan, dupTChan)
import Control.Concurrent.STM.TVar (readTVar, TVar, modifyTVar')
import qualified Data.IxSet as Ix
import Data.Maybe (fromJust)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Except (withExceptT, ExceptT)

type instance AuthServerData (AuthProtect "answerer-auth") = AnswererCookie
type instance AuthServerData (AuthProtect "questioner-auth") = QuestionerCookie
type instance AuthServerData (AuthProtect "any-auth") = AnyCookie

type SessionsAPI =
    ReqBody '[JSON] SessionReq :> Post '[JSON] (Headers '[Header "Set-Cookie" ByteString] Answerer)
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "lock" :> PostNoContent '[PlainText] NoContent
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> DeleteNoContent '[PlainText] NoContent
    :<|> ReqBody '[JSON] JoinReq :> Capture "session_id" SessionId :> "join" :> Post '[JSON] (Headers '[Header "Set-Cookie" ByteString] Questioner)
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> "ask" :> ReqBody '[JSON] QuestionReq :> Post '[JSON] Question
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "upvote" :> Post '[JSON] Question
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "answer" :> Post '[JSON] Question
    :<|> AuthProtect "any-auth" :> Capture "session_id" SessionId :> Get '[JSON] FullSession

sessionsAPI :: Proxy SessionsAPI
sessionsAPI = Proxy

sendSessionMessage :: MonadIO m => TVar SessionChannelDB -> SessionId -> (a -> SessionMessage) -> a -> ExceptT AppError m a
sendSessionMessage sessionChannelDB sessionId messageFn a = do
    r <- liftIO $ withSessionChannel sessionChannelDB sessionId
        (return $ Left SessionNotFound)
        (\ chan -> atomically (writeTChan chan (messageFn a)) >> return (Right a))
    case r of
        Left e -> throwError e
        Right a -> return a

appToServant = withExceptT withError

sessionsServer ::
    (AnswererCookie -> Answerer -> Handler (Headers '[Header "Set-Cookie" ByteString] Answerer)) ->
    (QuestionerCookie -> Questioner -> Handler (Headers '[Header "Set-Cookie" ByteString] Questioner)) ->
    AcidState App ->
    TVar SessionChannelDB ->
    Server SessionsAPI
sessionsServer addAnswererSession addQuestionerSession app sessionChannelDB = newSessionH :<|> lockSessionH :<|> deleteSessionH :<|> joinSessionH :<|> askQH :<|> upvoteQH :<|> answerQH :<|> fullSessionH
    where
        newSessionH req = do
            answerer@(Answerer id _ _) <- liftIO $ newSessionHandler app req sessionChannelDB
            addAnswererSession (AnswererCookie id) answerer
        lockSessionH c id = do
            appToServant $ sessionLockHandler app c id sessionChannelDB
            return NoContent
        deleteSessionH c id = do
            appToServant $ sessionDeleteHandler app c id sessionChannelDB
            return NoContent
        joinSessionH req sessionId = do
            questioner@(Questioner id _ _) <- appToServant $ sessionJoinHandler app req sessionId
            addQuestionerSession (QuestionerCookie id) questioner
        askQH cookie sessionId req = do
            question <- appToServant $ askQuestionHandler app cookie sessionId req sessionChannelDB
            return question
        upvoteQH cookie sessionId req = do
            question <- appToServant $  upvoteQuestionHandler app cookie sessionId req sessionChannelDB
            return question
        answerQH cookie sessionId req = do
            question <- appToServant $ answerQuestionHandler app cookie sessionId req sessionChannelDB
            return question
        fullSessionH cookie sessionId = appToServant $ fullSessionHandler app cookie sessionId

newSessionHandler :: AcidState App -> SessionReq -> TVar SessionChannelDB -> IO Answerer
newSessionHandler app (SessionReq name expiration answererName) sessionChannelDB = do
    answerer@(Answerer _ sId _) <- update app (NewSession name expiration answererName)
    sessionChan <- mkSessionChannel sId
    atomically $ modifyTVar' sessionChannelDB (Ix.insert sessionChan)
    return answerer

sessionLockHandler :: MonadIO m => AcidState App -> AnswererCookie -> SessionId -> TVar SessionChannelDB -> ExceptT AppError m ()
sessionLockHandler app (AnswererCookie id) sessionId sessionChannelDB = do
    withAppError <=< liftIO $ update app (LockSession id sessionId)
    sendSessionMessage sessionChannelDB sessionId (const SessionLocked) ()

sessionDeleteHandler :: MonadIO m => AcidState App -> AnswererCookie -> SessionId -> TVar SessionChannelDB -> ExceptT AppError m ()
sessionDeleteHandler app (AnswererCookie id) sessionId sessionChannelDB = do
    withAppError <=< liftIO $ update app (DeleteSession id sessionId)
    sendSessionMessage sessionChannelDB sessionId (const SessionClosed) ()

sessionJoinHandler :: MonadIO m => AcidState App -> JoinReq -> SessionId -> ExceptT AppError m Questioner
sessionJoinHandler app (JoinReq name) sessionId = withAppError <=< liftIO $ update app (JoinSession sessionId name)

askQuestionHandler :: MonadIO m => AcidState App -> QuestionerCookie -> SessionId -> QuestionReq -> TVar SessionChannelDB -> ExceptT AppError m Question
askQuestionHandler app (QuestionerCookie questionerId) sessionId (QuestionReq question) sessionChannelDB = do
    q <- withAppError <=< liftIO $ update app (AddQuestion question sessionId questionerId)
    sendSessionMessage sessionChannelDB sessionId QuestionAsked q
    return q

upvoteQuestionHandler :: MonadIO m => AcidState App -> QuestionerCookie -> SessionId -> QuestionId -> TVar SessionChannelDB -> ExceptT AppError m Question
upvoteQuestionHandler app (QuestionerCookie questionerId) sessionId questionId sessionChannelDB = do
    q <- withAppError <=< liftIO $ update app (UpvoteQuestion questionId sessionId questionerId)
    sendSessionMessage sessionChannelDB sessionId QuestionUpvoted q
    return q

answerQuestionHandler :: MonadIO m => AcidState App -> AnswererCookie -> SessionId -> QuestionId -> TVar SessionChannelDB -> ExceptT AppError m Question
answerQuestionHandler app (AnswererCookie  answererId) sessionId questionId sessionChannelDB = do
    q <- withAppError <=< liftIO $ update app (AnswerQuestion questionId sessionId answererId)
    sendSessionMessage sessionChannelDB sessionId QuestionAnswered q
    return q

fullSessionHandler :: MonadIO m => AcidState App -> AnyCookie -> SessionId -> ExceptT AppError m FullSession
fullSessionHandler app cookie sessionId = withAppError <=< liftIO $ query app (GetFullSession (anyCookieToIds cookie) sessionId)

instance ToParamSchema ByteString where
    toParamSchema _ = toParamSchema (Proxy :: Proxy String)
