{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts  #-}
module Modernator.SessionsAPI where

import Modernator.Types
import Modernator.Commands
import Modernator.APIUtils
import Modernator.RequestBodies
import Modernator.Cookies
import Servant
import Data.Proxy
import Data.Acid
import Data.Time.Clock
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))

type SessionsAPI =
    ReqBody '[JSON] SessionReq :> Post '[JSON] (Headers '[Header "Cookie" SessionCookie] Answerer)
    :<|> Header "Cookie" SessionCookie :> Capture "session_id" SessionId :> "lock" :> PostNoContent '[PlainText] NoContent
    :<|> Header "Cookie" SessionCookie :> Capture "session_id" SessionId :> DeleteNoContent '[PlainText] NoContent
    :<|> ReqBody '[JSON] JoinReq :> Capture "session_id" SessionId :> "join" :> Post '[JSON] (Headers '[Header "Cookie" SessionCookie] Questioner)
    :<|> Header "Cookie" SessionCookie :> Capture "session_id" SessionId :> "questions" :> "ask" :> ReqBody '[JSON] QuestionReq :> Post '[JSON] Question
    :<|> Header "Cookie" SessionCookie :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "upvote" :> Post '[JSON] Question
    :<|> Header "Cookie" SessionCookie :> Capture "session_id" SessionId :> "questions" :> Capture "question_id" QuestionId :> "answer" :> PostNoContent '[PlainText] NoContent

sessionsAPI :: Proxy SessionsAPI
sessionsAPI = Proxy

sessionsServer :: AcidState App -> Server SessionsAPI
sessionsServer app = newSessionH :<|> lockSessionH :<|> deleteSessionH :<|> joinSessionH :<|> askQH :<|> upvoteQH :<|> answerQH
    where
        newSessionH req = do
            answerer@(Answerer id _ _) <- liftIO . newSessionHandler app $ req
            return $ addHeader (SessionCookie (Left id)) answerer
        lockSessionH c id = withError <=< fmap (fmap unitToNoContent) . liftIO . sessionLockHandler app c $ id
        deleteSessionH c id = withError <=< fmap (fmap unitToNoContent) . liftIO . sessionDeleteHandler app c $ id
        joinSessionH req sessionId = do
            questioner@(Questioner id _ _) <- withError <=< liftIO . sessionJoinHandler app req $ sessionId
            return $ addHeader (SessionCookie (Right id)) questioner
        askQH cookie sessionId = withError <=< liftIO . askQuestionHandler app cookie sessionId
        upvoteQH cookie sessionId = withError <=< liftIO . upvoteQuestionHandler app cookie sessionId
        answerQH cookie sessionId = withError <=< fmap (fmap unitToNoContent) . liftIO . answerQuestionHandler app cookie sessionId

newSessionHandler :: AcidState App -> SessionReq -> IO Answerer
newSessionHandler app (SessionReq name expiration answererName) = update app (NewSession name expiration answererName)

sessionLockHandler :: AcidState App -> Maybe SessionCookie -> SessionId -> IO (Either AppError ())
sessionLockHandler _ Nothing _ = return $ Left NotAuthorizedForSession
sessionLockHandler _ (Just (SessionCookie (Right _))) _ = return $ Left MustBeAnswerer
sessionLockHandler app (Just (SessionCookie (Left id))) sessionId = update app (LockSession id sessionId)

sessionDeleteHandler :: AcidState App -> Maybe SessionCookie -> SessionId -> IO (Either AppError ())
sessionDeleteHandler _ Nothing _ = return $ Left NotAuthorizedForSession
sessionDeleteHandler _ (Just (SessionCookie (Right _))) _ = return $ Left MustBeAnswerer
sessionDeleteHandler app (Just (SessionCookie (Left id))) sessionId = update app (DeleteSession id sessionId)

sessionJoinHandler :: AcidState App -> JoinReq -> SessionId -> IO (Either AppError Questioner)
sessionJoinHandler app (JoinReq name) sessionId = update app (JoinSession sessionId name)

askQuestionHandler :: AcidState App -> Maybe SessionCookie -> SessionId -> QuestionReq -> IO (Either AppError Question)
askQuestionHandler _ Nothing _ _ = return $ Left NotAuthorizedForSession
askQuestionHandler _ (Just (SessionCookie (Left _))) _ _ = return $ Left MustBeQuestioner
askQuestionHandler app (Just (SessionCookie (Right questionerId))) sessionId (QuestionReq question) = update app (AddQuestion question sessionId questionerId)

upvoteQuestionHandler :: AcidState App -> Maybe SessionCookie -> SessionId -> QuestionId -> IO (Either AppError Question)
upvoteQuestionHandler _ Nothing _ _ = return $ Left NotAuthorizedForSession
upvoteQuestionHandler _ (Just (SessionCookie (Left _))) _ _ = return $ Left MustBeQuestioner
upvoteQuestionHandler app (Just (SessionCookie (Right questionerId))) sessionId questionId = update app (UpvoteQuestion questionId sessionId questionerId)

answerQuestionHandler :: AcidState App -> Maybe SessionCookie -> SessionId -> QuestionId -> IO (Either AppError ())
answerQuestionHandler _ Nothing _ _ = return $ Left NotAuthorizedForSession
answerQuestionHandler _ (Just (SessionCookie (Right _))) _ _ = return $ Left MustBeAnswerer
answerQuestionHandler app (Just (SessionCookie (Left answererId))) sessionId questionId = update app (AnswerQuestion questionId sessionId answererId)
