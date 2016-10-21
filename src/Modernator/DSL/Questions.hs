{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Modernator.DSL.Questions
(
) where

import Control.Monad.Free
import Data.Text
import Modernator.Types
import Modernator.DSL.Types

class Questions f where
    askQuestion :: Text -> Questioner -> f (Question Unanswered)
    answerQuestion :: Question Unanswered -> f (Question Answered)
    upvoteQuestion :: Question Unanswered -> f (Question Unanswered)
