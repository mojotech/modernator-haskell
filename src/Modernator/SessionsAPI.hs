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
import Data.Acid
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar')
import qualified Data.IxSet as Ix

type instance AuthServerData (AuthProtect "answerer-auth") = AnswererCookie
type instance AuthServerData (AuthProtect "questioner-auth") = QuestionerCookie
type instance AuthServerData (AuthProtect "any-auth") = AnyCookie

type SessionsAPI =
    Get '[JSON] [FullSession]
    :<|> ReqBody '[JSON] SessionReq :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] Answerer)
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "lock" :> PostNoContent '[PlainText] NoContent
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> DeleteNoContent '[PlainText] NoContent
    :<|> ReqBody '[JSON] JoinReq :> Capture "session_id" SessionId :> "join" :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] Questioner)
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> "ask" :> ReqBody '[JSON] QuestionReq :> Post '[JSON] Question
    :<|> AuthProtect "questioner-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "upvote" :> Post '[JSON] Question
    :<|> AuthProtect "answerer-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "answer" :> Post '[JSON] Question
    :<|> AuthProtect "any-auth" :> Capture "session_id" SessionId :> Get '[JSON] FullSession

sessionsAPI :: Proxy SessionsAPI
sessionsAPI = Proxy

sendSessionMessage :: TVar SessionChannelDB -> SessionId -> (a -> SessionMessage) -> a -> IO (Either AppError a)
sendSessionMessage sessionChannelDB sessionId messageFn a =
    withSessionChannel sessionChannelDB sessionId
        (return $ Left SessionNotFound)
        (\ chan -> atomically (writeTChan chan (messageFn a)) >> return (Right a))

sessionsServer ::
    (AnswererCookie -> Answerer -> Handler (Headers '[Header "Set-Cookie" EncryptedSession] Answerer)) ->
    (QuestionerCookie -> Questioner -> Handler (Headers '[Header "Set-Cookie" EncryptedSession] Questioner)) ->
    AcidState App ->
    TVar SessionChannelDB ->
    Server SessionsAPI
sessionsServer addAnswererSession addQuestionerSession app sessionChannelDB = allSessionsH :<|> newSessionH :<|> lockSessionH :<|> deleteSessionH :<|> joinSessionH :<|> askQH :<|> upvoteQH :<|> answerQH :<|> fullSessionH
    where
        allSessionsH = liftIO . allSessionsHandler $ app
        newSessionH req = do
            answerer@(Answerer id sId _) <- liftIO . newSessionHandler app $ req
            sessionChan <- liftIO $ mkSessionChannel sId
            liftIO $ atomically $ modifyTVar' sessionChannelDB (Ix.insert sessionChan)
            addAnswererSession (AnswererCookie id) answerer
        lockSessionH c id = do
            withError <=< liftIO . sessionLockHandler app c $ id
            withError <=< liftIO . sendSessionMessage sessionChannelDB id (const SessionLocked) $ ()
            return NoContent
        deleteSessionH c id = do
            withError <=< liftIO . sessionDeleteHandler app c $ id
            withError <=< liftIO . sendSessionMessage sessionChannelDB id (const SessionClosed) $ ()
            return NoContent
        joinSessionH req sessionId = do
            questioner@(Questioner id _ _) <- withError <=< liftIO . sessionJoinHandler app req $ sessionId
            withError <=< liftIO . sendSessionMessage sessionChannelDB sessionId QuestionerJoined $ questioner
            addQuestionerSession (QuestionerCookie id) questioner
        askQH cookie sessionId req = do
            question <- withError <=< liftIO . askQuestionHandler app cookie sessionId $ req
            withError <=< liftIO . sendSessionMessage sessionChannelDB sessionId QuestionAsked $ question
            return question
        upvoteQH cookie sessionId req = do
            question <- withError <=< liftIO . upvoteQuestionHandler app cookie sessionId $ req
            withError <=< liftIO . sendSessionMessage sessionChannelDB sessionId QuestionUpvoted $ question
            return question
        answerQH cookie sessionId req = do
            question <- withError <=< liftIO . answerQuestionHandler app cookie sessionId $ req
            withError <=< liftIO . sendSessionMessage sessionChannelDB sessionId QuestionAnswered $ question
            return question
        fullSessionH cookie = withError <=< liftIO . fullSessionHandler app cookie

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

fullSessionHandler :: AcidState App -> AnyCookie -> SessionId -> IO (Either AppError FullSession)
fullSessionHandler app cookie sessionId = query app (GetFullSession (anyCookieToIds cookie) sessionId)

allSessionsHandler :: AcidState App -> IO [FullSession]
allSessionsHandler app = query app GetAllSessions
