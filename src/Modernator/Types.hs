{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, FlexibleInstances, OverloadedStrings, OverloadedLists #-}
module Modernator.Types where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.IxSet hiding (Proxy)
import qualified Data.IxSet as Ix
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChan)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar)

-- | GHC.Generics used for deriving ToJSON instances
import GHC.Generics (Generic)

-- | Some typeclasses we'll need
import Web.HttpApiData (FromHttpApiData)
import Data.Serialize (Serialize)

-- | Keep track of any domain specific exceptions so we can translate them to
-- generic HTTP exceptions later.
data AppError = QuestionNotFound
              | NotAuthorizedForSession
              | MustBeAnswerer
              | MustBeQuestioner
              | AnswererNotFound
              | QuestionerNotFound
              | SessionNotFound
              | SessionAlreadyLocked
    deriving (Show, Generic, Eq, Ord, Enum, Bounded)

-- | A question has an id, number of votes, text, and answered status
data Question = Question QuestionId SessionId Votes Text Answered
    deriving (Show, Generic, Eq, Ord)

-- | A QuestionId is represented as an integer, but we should not be able to
-- perform arithmetic and other numeric operations on it, as it's not a number
-- in our domain, it's an identifier.
newtype QuestionId = QuestionId Integer
    deriving (Show, FromHttpApiData, Enum, Eq, Ord, Generic)

-- | The number of votes is represented as an integer. Like the QuestionId, it
-- also disallows arithmetic and other mathematical operations on it. In our
-- domain, votes can only be incremented, which is functionality provided by
-- the Enum typeclass.
newtype Votes = Votes Integer
    deriving (Show, Enum, Eq, Ord, Generic)

-- | A question's answered status is conceptually a boolean, but we use a more
-- descriptive type here for better documenting code.
data Answered = Answered | NotAnswered
    deriving (Show, Generic, Eq, Ord, Enum, Bounded)

-- | We represent the set of questions as an indexed set, which allows
-- efficient queries and updates using custom indexes; similar to SQL indexes.
-- Questions are indexed on id and answered status.
type QuestionDB = IxSet Question
instance Indexable Question where
    empty = ixSet [ ixFun (\ (Question id _ _ _ _) -> [id])
                  , ixFun (\ (Question _ _ _ _ a) -> [a])
                  , ixFun (\ (Question _ sid _ _ _) -> [sid])
                  ]
newtype AnswererId = AnswererId Integer
    deriving (Show, FromHttpApiData, Enum, Eq, Ord, Generic, Serialize)

data Answerer = Answerer AnswererId SessionId Text
    deriving (Show, Generic, Eq, Ord)

type AnswererDB = IxSet Answerer
instance Indexable Answerer where
    empty = ixSet [ ixFun (\ (Answerer id _ _) -> [id])
                  , ixFun (\ (Answerer _ sid _) -> [sid])
                  ]

newtype SessionId = SessionId Integer
    deriving (Generic, Show, FromHttpApiData, Enum, Eq, Ord)

data LockedStatus = Locked | Unlocked
    deriving (Show, Generic, Eq, Ord, Bounded, Enum)

data Session = Session SessionId Text (Maybe UTCTime) LockedStatus
    deriving (Show, Eq, Ord)

type SessionDB = IxSet Session
instance Indexable Session where
    empty = ixSet [ ixFun (\ (Session id _ _ _) -> [id])
                  , ixFun (\ (Session _ _ _ locked) -> [locked])
                  ]

newtype QuestionerId = QuestionerId Integer
    deriving (Show, FromHttpApiData, Enum, Eq, Ord, Generic, Serialize)

data Questioner = Questioner QuestionerId SessionId (Maybe Text)
    deriving (Show, Generic, Eq, Ord)

type QuestionerDB = IxSet Questioner
instance Indexable Questioner where
    empty = ixSet [ ixFun (\ (Questioner id _ _) -> [id])
                  , ixFun (\ (Questioner _ sid _) -> [sid])
                  ]

-- | The state of our application. This is persisted using AcidState and handed
-- to each request handler.
data App = App
    { questions :: QuestionDB
    , nextQuestionId :: QuestionId
    , answerers :: AnswererDB
    , nextAnswererId :: AnswererId
    , sessions :: SessionDB
    , nextSessionId :: SessionId
    , questioners :: QuestionerDB
    , nextQuestionerId :: QuestionerId
    }
    deriving (Show)

-- | The default application state, an empty set of questions.
emptyApp = App
    { questions = empty
    , nextQuestionId = QuestionId 1
    , answerers = empty
    , nextAnswererId = AnswererId 1
    , sessions = empty
    , nextSessionId = SessionId 1
    , questioners = empty
    , nextQuestionerId = QuestionerId 1
    }

-- | A helper to upvote a question.
upvote :: Question -> Question
upvote (Question id sid v t a) = Question id sid (succ v) t a

-- | A helper to answer a question. Note that we don't specify that only
-- unanswered questions can be answered. Fortunately this is idempotent now so
-- it doesn't matter, but if we add more logic around answering questions that
-- constraint might need tightening.
answer :: Question -> Question
answer (Question id sid v t _) = Question id sid v t Answered

data FullSession = FullSession
    { fullSession_session :: Session
    , fullSession_answerer :: Answerer
    , fullSession_questioners :: [Questioner]
    , fullSession_questions :: [Question]
    }
    deriving (Show, Eq)

getFullSessionFromApp :: App -> SessionId -> Maybe FullSession
getFullSessionFromApp  app sessionId =
    FullSession
        <$> (Ix.getOne . Ix.getEQ sessionId . sessions $ app)
        <*> (Ix.getOne . Ix.getEQ sessionId . answerers $ app)
        <*> pure (Ix.toList . Ix.getEQ sessionId . questioners $ app)
        <*> pure (Ix.toList . Ix.getEQ sessionId . questions $ app)

-- | Our Websocket message type. SessionExceptionMessages should be more specific than AppError
data SessionMessage = SessionLocked | SessionExpired | SessionClosed | SessionExceptionMessage AppError | QuestionAsked Question | QuestionUpvoted Question | QuestionAnswered Question | SessionState FullSession
    deriving (Show, Eq, Generic)

mkSessionChannel sessionId = do
    chan <- atomically newBroadcastTChan
    return $ SessionChannel (sessionId, chan)

newtype SessionChannel = SessionChannel (SessionId, TChan SessionMessage)
    deriving (Eq)

-- | For our "database" of session channels
type SessionChannelDB = IxSet SessionChannel
instance Indexable SessionChannel where
    empty = ixSet [ ixFun (\ (SessionChannel (id, _)) -> [id])
                  ]
instance Ord SessionChannel where
    (SessionChannel (sId, _)) <= (SessionChannel (sId', _)) = sId <= sId'

withSessionChannel :: TVar SessionChannelDB -> SessionId -> IO a -> (TChan SessionMessage -> IO a) -> IO a
withSessionChannel sessionChannelDB sessionId no yes = do
    sessionChannels <- atomically $ readTVar sessionChannelDB
    let channelM = Ix.getOne (Ix.getEQ sessionId sessionChannels)
    case channelM of
        Nothing -> no
        Just (SessionChannel (_, channel)) -> yes channel
