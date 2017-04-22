{-# LANGUAGE TemplateHaskell #-}
module Modernator.Migrations where

import Modernator.Types
import Data.SafeCopy

-- | We need to generate some SafeCopy instances so we can persist our
-- structures to disk.
$(deriveSafeCopy 1 'base ''AppError)
$(deriveSafeCopy 1 'base ''Answered)
$(deriveSafeCopy 1 'base ''QuestionId)
$(deriveSafeCopy 1 'base ''Votes)
$(deriveSafeCopy 1 'base ''Question)
$(deriveSafeCopy 1 'base ''Questioner)
$(deriveSafeCopy 1 'base ''QuestionerId)
$(deriveSafeCopy 1 'base ''Session)
$(deriveSafeCopy 1 'base ''SessionId)
$(deriveSafeCopy 1 'base ''LockedStatus)
$(deriveSafeCopy 1 'base ''Answerer)
$(deriveSafeCopy 1 'base ''AnswererId)
$(deriveSafeCopy 1 'base ''FullSession)
$(deriveSafeCopy 1 'base ''UserId)
$(deriveSafeCopy 1 'base ''Password)
$(deriveSafeCopy 1 'base ''User)
$(deriveSafeCopy 1 'base ''FullUser)

$(deriveSafeCopy 1 'base ''App)
