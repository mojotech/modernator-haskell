{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies #-}
module Modernator.SessionsAPI where

import Modernator.Types
import Modernator.Commands
import Modernator.APIUtils
import Modernator.RequestBodies
import Modernator.Cookies
import Modernator.APIAuth()
import Servant
import Data.Acid
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar')
import qualified Data.IxSet as Ix

type SessionsAPI =
    Get '[JSON] [FullSession]
    :<|> AuthProtect "user-auth" :> ReqBody '[JSON] SessionReq :> Post '[JSON] Session
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> "lock" :> PostNoContent '[PlainText] NoContent
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> DeleteNoContent '[PlainText] NoContent
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> "join" :> Post '[JSON] User
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> "questions" :> "ask" :> ReqBody '[JSON] QuestionReq :> Post '[JSON] Question
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "upvote" :> Post '[JSON] Question
    :<|> AuthProtect "user-auth" :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "answer" :> Post '[JSON] Question
    :<|> Capture "session_id" SessionId :> Get '[JSON] FullSession

sessionsAPI :: Proxy SessionsAPI
sessionsAPI = Proxy

sendSessionMessage :: TVar SessionChannelDB -> SessionId -> (a -> SessionMessage) -> a -> IO (Either AppError a)
sendSessionMessage sessionChannelDB sessionId messageFn a =
    withSessionChannel sessionChannelDB sessionId
        (return $ Left SessionNotFound)
        (\ chan -> atomically (writeTChan chan (messageFn a)) >> return (Right a))

sessionsServer ::
    AcidState App ->
    TVar SessionChannelDB ->
    Server SessionsAPI
sessionsServer app sessionChannelDB = allSessionsH :<|> newSessionH :<|> lockSessionH :<|> deleteSessionH :<|> joinSessionH :<|> askQH :<|> upvoteQH :<|> answerQH :<|> fullSessionH
    where
        allSessionsH = liftIO . allSessionsHandler $ app
        newSessionH cookie req = do
            session@(Session sId _ _ _) <- withError <=< liftIO . newSessionHandler app req $ cookie
            sessionChan <- liftIO $ mkSessionChannel sId
            liftIO $ atomically $ modifyTVar' sessionChannelDB (Ix.insert sessionChan)
            return session
        lockSessionH c id = do
            withError <=< liftIO . sessionLockHandler app c $ id
            withError <=< liftIO . sendSessionMessage sessionChannelDB id (const SessionLocked) $ ()
            return NoContent
        deleteSessionH c id = do
            withError <=< liftIO . sessionDeleteHandler app c $ id
            withError <=< liftIO . sendSessionMessage sessionChannelDB id (const SessionClosed) $ ()
            return NoContent
        joinSessionH cookie sessionId = do
            user <- withError <=< liftIO . sessionJoinHandler app cookie $ sessionId
            withError <=< liftIO . sendSessionMessage sessionChannelDB sessionId QuestionerJoined $ user
            return user
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
        fullSessionH = withError <=< liftIO . fullSessionHandler app

newSessionHandler :: AcidState App -> SessionReq -> ModernatorCookie -> IO (Either AppError Session)
newSessionHandler app (SessionReq name expiration) (ModernatorCookie userId) = update app (NewSession name expiration userId)

sessionLockHandler :: AcidState App -> ModernatorCookie -> SessionId -> IO (Either AppError ())
sessionLockHandler app (ModernatorCookie userId) sessionId = update app (LockSession userId sessionId)

sessionDeleteHandler :: AcidState App -> ModernatorCookie -> SessionId -> IO (Either AppError ())
sessionDeleteHandler app (ModernatorCookie userId) sessionId = update app (DeleteSession userId sessionId)

sessionJoinHandler :: AcidState App -> ModernatorCookie -> SessionId -> IO (Either AppError User)
sessionJoinHandler app (ModernatorCookie uId) sessionId = update app (JoinSession sessionId uId)

askQuestionHandler :: AcidState App -> ModernatorCookie -> SessionId -> QuestionReq -> IO (Either AppError Question)
askQuestionHandler app (ModernatorCookie userId) sessionId (QuestionReq question) = update app (AddQuestion question sessionId userId)

upvoteQuestionHandler :: AcidState App -> ModernatorCookie -> SessionId -> QuestionId -> IO (Either AppError Question)
upvoteQuestionHandler app (ModernatorCookie userId) sessionId questionId = update app (UpvoteQuestion questionId sessionId userId)

answerQuestionHandler :: AcidState App -> ModernatorCookie -> SessionId -> QuestionId -> IO (Either AppError Question)
answerQuestionHandler app (ModernatorCookie userId) sessionId questionId = update app (AnswerQuestion questionId sessionId userId)

fullSessionHandler :: AcidState App -> SessionId -> IO (Either AppError FullSession)
fullSessionHandler app sessionId = query app (GetFullSession sessionId)

allSessionsHandler :: AcidState App -> IO [FullSession]
allSessionsHandler app = query app GetAllSessions
