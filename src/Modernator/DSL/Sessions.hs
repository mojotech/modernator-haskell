{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Modernator.DSL.Sessions
(
) where

import Control.Monad.Free
import Data.Text
import Data.Time.Clock (UTCTime)
import Modernator.Types
import Modernator.DSL.Types

class Sessions f where
    createSession :: Text -> Maybe UTCTime -> f (Session Unlocked)
    deleteSession :: Session status -> f ()
    lockSession :: Session Unlocked -> f (Session Locked)
    listSessions :: f [FullSession status]
    viewSession :: f (FullSession status)
