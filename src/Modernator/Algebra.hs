{-# LANGUAGE DeriveFunctor, OverloadedStrings, ExistentialQuantification #-}
module Modernator.Algebra where

import Control.Monad.Free
import Data.Text
import Data.Time.Clock (UTCTime)
import Data.Functor.Sum
import Control.Monad.Identity

data Questioner = Questioner
  deriving (Show)
data Session s = Session
  deriving (Show)
data FullSession s = FullSession (Session s)
  deriving (Show)
data Question s = Question
  deriving (Show)

data Answered
data Unanswered
data Locked
data Unlocked

class Questions f where
    askQuestion :: Text -> Questioner -> f (Question Unanswered)
    answerQuestion :: Question Unanswered -> f (Question Answered)
    upvoteQuestion :: Question Unanswered -> f (Question Unanswered)
