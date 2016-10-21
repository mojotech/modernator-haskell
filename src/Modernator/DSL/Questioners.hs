{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Modernator.DSL.Questioners
(
  joinSession,
  deleteQuestioner
) where

import Control.Monad.Free
import Data.Text
import Modernator.Types
import Modernator.DSL.Types

class Questioners f where
    joinSession :: Session Unlocked -> Text -> f Questioner
    deleteQuestioner :: Questioner -> f ()

data Questioners
