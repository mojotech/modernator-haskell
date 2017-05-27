{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Modernator.Migrations where

import Modernator.Types
import Data.SafeCopy
import qualified Data.IxSet as Ix
import Data.IxSet (IxSet)
import Data.Text (Text)

newtype AnswererId = AnswererId Integer
    deriving (Eq, Ord)

data Answerer = Answerer AnswererId SessionId Text
    deriving (Eq, Ord)

type AnswererDB = IxSet Answerer
instance Ix.Indexable Answerer where
    empty = Ix.ixSet []

newtype QuestionerId = QuestionerId Integer
    deriving (Eq, Ord)

data Questioner = Questioner QuestionerId SessionId (Maybe Text)
    deriving (Eq, Ord)

type QuestionerDB = IxSet Questioner
instance Ix.Indexable Questioner where
    empty = Ix.ixSet []

newtype Votes' = Votes' [QuestionerId]

data App' = App' QuestionDB QuestionId AnswererDB AnswererId SessionDB SessionId QuestionerDB QuestionerId

$(deriveSafeCopy 1 'base ''Questioner)
$(deriveSafeCopy 1 'base ''QuestionerId)
$(deriveSafeCopy 1 'base ''Answerer)
$(deriveSafeCopy 1 'base ''AnswererId)
$(deriveSafeCopy 1 'base ''Votes')
$(deriveSafeCopy 1 'base ''AppError)
$(deriveSafeCopy 1 'base ''Answered)
$(deriveSafeCopy 1 'base ''QuestionId)
$(deriveSafeCopy 1 'base ''Question)
$(deriveSafeCopy 1 'base ''Session)
$(deriveSafeCopy 1 'base ''SessionId)
$(deriveSafeCopy 1 'base ''LockedStatus)
$(deriveSafeCopy 1 'base ''FullSession)

$(deriveSafeCopy 1 'base ''App')

$(deriveSafeCopy 1 'base ''UserId)
$(deriveSafeCopy 1 'base ''Password)
$(deriveSafeCopy 1 'base ''User)
$(deriveSafeCopy 1 'base ''FullUser)
$(deriveSafeCopy 1 'base ''QuestionerSessions)
$(deriveSafeCopy 1 'base ''AnswererSessions)
$(deriveSafeCopy 2 'extension ''Votes)
$(deriveSafeCopy 2 'extension ''App)

-- There's a semantic change in that anonymous users aren't really allowed
-- anymore, so I'm just clearing everything for this migration.
instance Migrate Votes where
    type MigrateFrom Votes = Votes'
    migrate _ = Votes []

instance Migrate App where
    type MigrateFrom App = App'
    migrate _ = App Ix.empty (QuestionId 0) Ix.empty (SessionId 0) Ix.empty (UserId 0)
