{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Modernator.Commands where

import Modernator.Migrations()
import Modernator.Types
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.IxSet as Ix
import Control.Monad.Trans.Either
import Data.Maybe (mapMaybe)

-- | Add a question. New questions start unanswered and with no votes.
addQuestion :: Text -> SessionId -> QuestionerId -> Update App (Either AppError Question)
addQuestion qtext sessionId questionerId = do
    app <- get
    nextId <- gets nextQuestionId
    qs <- gets questions
    let questionerM = Ix.getOne (Ix.getEQ questionerId (questioners app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    question <- runEitherT $ do  -- presence checks
        questioner <- hoistEither $ maybe (Left QuestionerNotFound) Right questionerM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        -- authorization
        hoistEither $ joined questioner session
        -- session can't be locked already
        hoistEither $ sessionUnlocked session
        let question = Question nextId sessionId (Votes 0) qtext NotAnswered
        return question
    case question of
        Right question -> do
            let newState = app
                    { questions = Ix.insert question qs
                    , nextQuestionId = (succ nextId)
                    }
            put newState
            return $ Right question
        Left e -> return $ Left e

questionInSession :: Question -> Session -> Either AppError ()
questionInSession (Question _ id _ _ _) (Session id' _ _ _) = if id == id' then Right () else Left NotAuthorizedForSession

upvoteQuestion :: QuestionId -> SessionId -> QuestionerId -> Update App (Either AppError Question)
upvoteQuestion questionId sessionId questionerId = do
    app <- get
    let questionM = Ix.getOne (Ix.getEQ questionId (questions app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
        questionerM = Ix.getOne (Ix.getEQ questionerId (questioners app))
    newQuestion <- runEitherT $ do
        questioner <- hoistEither $ maybe (Left QuestionerNotFound) Right questionerM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        question <- hoistEither $ maybe (Left QuestionNotFound) Right questionM
        hoistEither $ questionInSession question session
        hoistEither $ joined questioner session
        hoistEither $ sessionUnlocked session
        return $ upvote question
    case newQuestion of
        Right question -> do
            let newState = app { questions = Ix.updateIx questionId question (questions app) }
            put newState
            return $ Right question
        Left e -> return $ Left e

answerQuestion :: QuestionId -> SessionId -> AnswererId -> Update App (Either AppError Question)
answerQuestion questionId sessionId answererId = do
    app <- get
    let questionM = Ix.getOne (Ix.getEQ questionId (questions app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
        answererM = Ix.getOne (Ix.getEQ answererId (answerers app))
    newQuestion <- runEitherT $ do
        answerer <- hoistEither $ maybe (Left AnswererNotFound) Right answererM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        question <- hoistEither $ maybe (Left QuestionNotFound) Right questionM
        hoistEither $ questionInSession question session
        hoistEither $ authorized answerer session
        hoistEither $ sessionUnlocked session
        return $ answer question
    case newQuestion of
        Right question -> do
            let newState = app { questions = Ix.updateIx questionId question (questions app) }
            put newState
            return $ Right question
        Left e -> return $ Left e

newSession :: Text -> Maybe UTCTime -> Text -> Update App Answerer
newSession name expiration answererName = do
    app <- get
    let newSession = Session (nextSessionId app) name expiration Unlocked
        newAnswerer = Answerer (nextAnswererId app) (nextSessionId app) answererName
        newState = app
            { sessions = Ix.insert newSession (sessions app)
            , answerers = Ix.insert newAnswerer (answerers app)
            , nextSessionId = succ (nextSessionId app)
            , nextAnswererId = succ (nextAnswererId app)
            }
    put newState
    return newAnswerer

joined :: Questioner -> Session -> Either AppError ()
joined (Questioner _ sessionId _) (Session sessionId' _ _ _) = if sessionId == sessionId' then Right () else Left NotAuthorizedForSession

authorized :: Answerer -> Session -> Either AppError ()
authorized (Answerer _ sessionId _) (Session sessionId' _ _ _) = if sessionId == sessionId' then Right () else Left NotAuthorizedForSession

sessionUnlocked :: Session -> Either AppError ()
sessionUnlocked (Session _ _ _ Locked) = Left SessionAlreadyLocked
sessionUnlocked (Session _ _ _ Unlocked) = Right ()

lockSession' :: Session -> Session
lockSession' (Session id name exp _) = Session id name exp Locked

lockSession :: AnswererId -> SessionId -> Update App (Either AppError ())
lockSession answererId sessionId = do
    app <- get
    let answererM = Ix.getOne (Ix.getEQ answererId (answerers app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    newState <- runEitherT $ do  -- presence checks
        answerer <- hoistEither $ maybe (Left AnswererNotFound) Right answererM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        -- authorization
        hoistEither $ authorized answerer session
        -- session can't be locked already
        hoistEither $ sessionUnlocked session
        return app { sessions = Ix.updateIx sessionId (lockSession' session) (sessions app) }
    case newState of
        Right s -> do
            put s
            return $ Right ()
        Left e -> return $ Left e

deleteSession :: AnswererId -> SessionId -> Update App (Either AppError ())
deleteSession answererId sessionId = do
    app <- get
    let answererM = Ix.getOne (Ix.getEQ answererId (answerers app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    newState <- runEitherT $ do  -- presence checks
        answerer <- hoistEither $ maybe (Left AnswererNotFound) Right answererM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        -- authorization
        hoistEither $ authorized answerer session
        return app { sessions = Ix.deleteIx sessionId (sessions app)
                   , answerers = Ix.deleteIx sessionId (answerers app)
                   , questioners = Ix.getLT sessionId (questioners app) Ix.||| Ix.getGT sessionId (questioners app)
                   , questions = Ix.getLT sessionId (questions app) Ix.||| Ix.getGT sessionId (questions app)
                   }
    case newState of
        Right s -> do
            put s
            return $ Right ()
        Left e -> return $ Left e

joinSession :: SessionId -> Maybe Text -> Update App (Either AppError Questioner)
joinSession sessionId name = do
    ss <- gets sessions
    nextId <- gets nextQuestionerId
    qs <- gets questioners
    case Ix.getOne (Ix.getEQ sessionId ss) of
        Nothing -> return (Left SessionNotFound)
        Just _ -> do
            let questioner = Questioner nextId sessionId name
            modify (\ a -> a { questioners = (Ix.insert questioner qs), nextQuestionerId = succ nextId})
            return (Right questioner)

getState :: Query App App
getState = ask

doAuthorizeAnswerer :: App -> SessionId -> AnswererId -> Either AppError ()
doAuthorizeAnswerer app sId aId = do
    let answererM = Ix.getOne (Ix.getEQ aId (answerers app))
        sessionM = Ix.getOne (Ix.getEQ sId (sessions app))
    answerer <- maybe (Left AnswererNotFound) Right answererM
    session <- maybe (Left SessionNotFound) Right sessionM
    authorized answerer session
    return ()

doAuthorizeQuestioner :: App -> SessionId -> QuestionerId -> Either AppError ()
doAuthorizeQuestioner app sId qId = do
    let questionerM = Ix.getOne (Ix.getEQ qId (questioners app))
        sessionM = Ix.getOne (Ix.getEQ sId (sessions app))
    questioner <- maybe (Left QuestionerNotFound) Right questionerM
    session <- maybe (Left SessionNotFound) Right sessionM
    joined questioner session
    return ()

getFullSession :: Either AnswererId QuestionerId -> SessionId -> Query App (Either AppError FullSession)
getFullSession authId sId = do
    app <- ask
    let authed = either (doAuthorizeAnswerer app sId) (doAuthorizeQuestioner app sId) authId
    runEitherT $ do
        hoistEither $ authed
        hoistEither $ maybe (Left SessionNotFound) Right $ getFullSessionFromApp app sId

getAllSessions :: Query App [FullSession]
getAllSessions = do
    app <- ask
    return $ mapMaybe (getFullSessionFromApp app) . map (\ (Session id _ _ _) -> id) . Ix.toList . sessions $ app

$(makeAcidic ''App ['addQuestion, 'upvoteQuestion, 'answerQuestion, 'newSession, 'lockSession, 'getState, 'deleteSession, 'joinSession, 'getFullSession, 'getAllSessions])
