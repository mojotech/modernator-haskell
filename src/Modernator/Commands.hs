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
addQuestion :: Text -> SessionId -> UserId -> Update App (Either AppError Question)
addQuestion qtext sessionId userId = do
    app <- get
    nextId <- gets nextQuestionId
    qs <- gets questions
        -- authorization
    let userM = fmap fullUserUser $ Ix.getOne (Ix.getEQ (QuestionerSessionId sessionId) $ Ix.getEQ userId (users app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    question <- runEitherT $ do  -- presence checks
        hoistEither $ maybe (Left UserNotFound) Right userM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        -- session can't be locked already
        hoistEither $ sessionUnlocked session
        let question = Question nextId sessionId (Votes []) qtext NotAnswered
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

questionNotUpvotedByQuestioner :: UserId -> Question -> Either AppError ()
questionNotUpvotedByQuestioner uId (Question _ _ (Votes vs) _ _) = if uId `elem` vs then Left QuestionAlreadyUpvoted else Right ()

upvoteQuestion :: QuestionId -> SessionId -> UserId -> Update App (Either AppError Question)
upvoteQuestion questionId sessionId userId = do
    app <- get
    let questionM = Ix.getOne (Ix.getEQ questionId (questions app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
        userM = Ix.getOne (Ix.getEQ (QuestionerSessionId sessionId) $ Ix.getEQ userId (users app))
    newQuestion <- runEitherT $ do
        hoistEither $ maybe (Left UserNotFound) Right userM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        question <- hoistEither $ maybe (Left QuestionNotFound) Right questionM
        hoistEither $ questionInSession question session
        hoistEither $ sessionUnlocked session
        hoistEither $ questionNotUpvotedByQuestioner userId question
        return $ upvote userId question
    case newQuestion of
        Right question -> do
            let newState = app { questions = Ix.updateIx questionId question (questions app) }
            put newState
            return $ Right question
        Left e -> return $ Left e

answerQuestion :: QuestionId -> SessionId -> UserId -> Update App (Either AppError Question)
answerQuestion questionId sessionId userId = do
    app <- get
    let questionM = Ix.getOne (Ix.getEQ questionId (questions app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
        userM = Ix.getOne (Ix.getEQ (AnswererSessionId sessionId) $ Ix.getEQ userId (users app))
    newQuestion <- runEitherT $ do
        hoistEither $ maybe (Left UserNotFound) Right userM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        question <- hoistEither $ maybe (Left QuestionNotFound) Right questionM
        hoistEither $ questionInSession question session
        hoistEither $ sessionUnlocked session
        return $ answer question
    case newQuestion of
        Right question -> do
            let newState = app { questions = Ix.updateIx questionId question (questions app) }
            put newState
            return $ Right question
        Left e -> return $ Left e

newSession :: Text -> Maybe UTCTime -> UserId -> Update App (Either AppError Session)
newSession name expiration userId = do
    app <- get
    newState <- runEitherT $ do
        (FullUser user p) <- hoistEither $ maybe (Left UserNotFound) Right $ Ix.getOne (Ix.getEQ userId (users app))
        let newSession = Session newSessionId name expiration Unlocked
            newSessionId = nextSessionId app
            newUser = FullUser (addAnswererSession newSessionId user) p
            newState = app
                { sessions = Ix.insert newSession (sessions app)
                , users = Ix.updateIx userId newUser (users app)
                , nextSessionId = succ newSessionId
                }
        return (newSession, newState)
    case newState of
        Right (newSession, s) -> do
            put s
            return $ Right newSession
        Left e -> return $ Left e

sessionUnlocked :: Session -> Either AppError ()
sessionUnlocked (Session _ _ _ Locked) = Left SessionAlreadyLocked
sessionUnlocked (Session _ _ _ Unlocked) = Right ()

lockSession' :: Session -> Session
lockSession' (Session id name exp _) = Session id name exp Locked

lockSession :: UserId -> SessionId -> Update App (Either AppError ())
lockSession userId sessionId = do
    app <- get
    let userM = Ix.getOne (Ix.getEQ (AnswererSessionId sessionId) $ Ix.getEQ userId (users app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    newState <- runEitherT $ do  -- presence checks
        hoistEither $ maybe (Left UserNotFound) Right userM
        session <- hoistEither $ maybe (Left SessionNotFound) Right sessionM
        -- session can't be locked already
        hoistEither $ sessionUnlocked session
        return app { sessions = Ix.updateIx sessionId (lockSession' session) (sessions app) }
    case newState of
        Right s -> do
            put s
            return $ Right ()
        Left e -> return $ Left e

deleteSession :: UserId -> SessionId -> Update App (Either AppError ())
deleteSession userId' sessionId = do
    app <- get
    let userM = Ix.getOne (Ix.getEQ (AnswererSessionId sessionId) $ Ix.getEQ userId' (users app))
        sessionM = Ix.getOne (Ix.getEQ sessionId (sessions app))
    newState <- runEitherT $ do  -- presence checks
        hoistEither $ maybe (Left UserNotFound) Right userM
        hoistEither $ maybe (Left SessionNotFound) Right sessionM
        let allUsers = Ix.toList $ Ix.getEQ sessionId (users app)
            updatedUsers = map (\ (FullUser u p) -> (FullUser (removeSession sessionId u) p)) allUsers
        return app { sessions = Ix.deleteIx sessionId (sessions app)
                   , users = foldr (\ u i -> Ix.updateIx (userId . fullUserUser $ u) u i) (users app) updatedUsers
                   , questions = Ix.getLT sessionId (questions app) Ix.||| Ix.getGT sessionId (questions app)
                   }
    case newState of
        Right s -> do
            put s
            return $ Right ()
        Left e -> return $ Left e

joinSession :: SessionId -> UserId -> Update App (Either AppError User)
joinSession sessionId userId = do
    app <- get
    newUser <- runEitherT $ do
        (FullUser user p) <- hoistEither $ maybe (Left UserNotFound) Right $ Ix.getOne $ Ix.getEQ userId (users app)
        hoistEither $ maybe (Left SessionNotFound) Right $ Ix.getOne $ Ix.getEQ sessionId (sessions app)
        return $ FullUser (addQuestionerSession sessionId user) p
    case newUser of
        Right u -> do
            modify (\ a -> a { users = (Ix.updateIx userId u (users app)) })
            return (Right (fullUserUser u))
        Left e -> return $ Left e

getState :: Query App App
getState = ask

getFullSession :: UserId -> SessionId -> Query App (Either AppError FullSession)
getFullSession userId sId = do
    app <- ask
    let userM = Ix.getOne (Ix.getEQ sId $ Ix.getEQ userId (users app))
    runEitherT $ do
        hoistEither $ maybe (Left UserNotFound) Right userM
        hoistEither $ maybe (Left SessionNotFound) Right $ getFullSessionFromApp app sId

getAllSessions :: Query App [FullSession]
getAllSessions = do
    app <- ask
    return $ mapMaybe (getFullSessionFromApp app) . map (\ (Session id _ _ _) -> id) . Ix.toList . sessions $ app

getMeForSession :: UserId -> SessionId -> Query App (Either AppError User)
getMeForSession userId sId = do
    app <- ask
    let userM = Ix.getOne (Ix.getEQ sId $ Ix.getEQ userId (users app))
    runEitherT $ do
        user <- hoistEither $ maybe (Left UserNotFound) Right userM
        return (fullUserUser user)

newUser :: Text -> Text -> Update App User
newUser name password = do
    us <- gets users
    nextId <- gets nextUserId
    let user = User nextId name (AnswererSessions []) (QuestionerSessions [])
        fullUser = FullUser user (hashPassword password)
    modify (\ a -> a { users = (Ix.insert fullUser us), nextUserId = succ nextId })
    return user

getUser :: UserId -> Query App (Either AppError User)
getUser id = do
    app <- ask
    case Ix.getOne (Ix.getEQ id (users app)) of
        Nothing -> return (Left UserNotFound)
        Just u -> return (Right (fullUserUser u))

authenticateUser :: Text -> Password -> Query App (Either AppError User)
authenticateUser name password = do
    app <- ask
    case Ix.getOne (Ix.getEQ name (users app)) of
        Nothing -> return (Left InvalidCredentials)
        Just u -> return $ if verifyUserPassword password u then Right (fullUserUser u) else Left InvalidCredentials

$(makeAcidic ''App ['addQuestion, 'upvoteQuestion, 'answerQuestion, 'newSession, 'lockSession, 'getState, 'deleteSession, 'joinSession, 'getFullSession, 'getAllSessions, 'getMeForSession, 'newUser, 'getUser, 'authenticateUser])
