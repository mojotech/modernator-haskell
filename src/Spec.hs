{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators, DataKinds, FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
module Main where

import Servant.Swagger.Test
import Test.Hspec
import Modernator.API
import Modernator.WebsocketsAPI
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Modernator.RequestBodies
import Modernator.Types
import Servant.Aeson.GenericSpecs (apiRoundtripSpecs, apiGoldenSpecs)
import Servant.API
import Servant.Aeson.Internal
import GHC.TypeLits
import Data.Proxy
import Modernator.QuickCheck

-- These instances are necessary because they're missing from the servant-aeson-specs library
instance MkTypeSpecs a => HasGenericSpecs (Verb (method :: StdMethod) returnStatus contentTypes (Headers hs a)) where
    collectRoundtripSpecs _ = mkTypeSpecs (Proxy :: Proxy a)

instance HasGenericSpecs rest  => HasGenericSpecs (AuthProtect (sym :: Symbol) :> rest) where
    collectRoundtripSpecs _ = collectRoundtripSpecs (Proxy :: Proxy rest)

instance HasGenericSpecs rest  => HasGenericSpecs (Capture (sym :: Symbol) x :> rest) where
    collectRoundtripSpecs _ = collectRoundtripSpecs (Proxy :: Proxy rest)

-- This covers types for the Raw Websocket endpoint
instance MkTypeSpecs a => HasGenericSpecs (Websocket a) where
    collectRoundtripSpecs _ = mkTypeSpecs (Proxy :: Proxy a)

spec :: Spec
spec = describe "Swagger" $ do
  context "ToJSON matches ToSchema for non-websockets APIs" $ validateEveryToJSON basicAPI
  context "ToJSON matches ToSchema for backup websockets API" $ validateEveryToJSON backupAPI
  context "ToJSON . FromJSON $ x = x for non-websockets APIs" $ apiRoundtripSpecs basicAPI
  context "ToJSON . FromJSON $ x = x for backup websockets API" $ apiRoundtripSpecs backupAPI
  context "JSON hasn't changed for non-websockets APIs" $ apiGoldenSpecs basicAPI
  context "JSON hasn't changed for backup websockets API" $ apiGoldenSpecs backupAPI

main = hspec $ do
    spec
    sessionsAPISpec

sessionsAPISpec :: Spec
sessionsAPISpec = describe "SessionsAPI" $ do
    creatingANewSession
    lockingASession
    deletingASession
    joiningASession
    askingAQuestion
    upvotingAQuestion
    answeringAQuestion
    gettingFullSession

-- This should really be replaced by a simple call to the handler, so that any
-- signature changes need to be reflected in the tests.
fulfilledByTypes = True

creatingANewSession = do
    describe "When creating a new session" $ do
        describe "Authorization" $ do
            it "doesn't require authorization" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
            it "cannot be done by Questioners" fulfilledByTypes
        it "creates a new session" pending
        it "creates a channel for the session" pending
        it "authenticates the requester as an Answerer for the session" pending

lockingASession = do
    describe "When locking a session" $ do
        describe "Authorization" $ do
            it "can be done by the Answerer for the session" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
        it "session must exist" pending
        it "locks the session" pending
        it "does not modify any other session" pending

deletingASession = do
    describe "When deleting a session" $ do
        describe "Authorization" $ do
            it "can be done by the Answerer for the session" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
        it "session must exist" pending
        it "deletes the session" pending
        it "does not modify any other session" pending

joiningASession = do
    describe "When joining a session" $ do
        describe "Authorization" $ do
            it "doesn't require authorization" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
            it "cannot be done by Questioners" fulfilledByTypes
        it "authenticates the requester as a Questioner for the session" pending
        it "does not modify any other session" pending
        it "session must exist" pending

askingAQuestion = do
    describe "When asking a question in a session" $ do
        describe "Authorization" $ do
            it "can be done by a Questioner in the session" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
        it "session must exist" pending
        it "creates a new question in the session" pending
        it "does not modify any other session" pending

upvotingAQuestion = do
    describe "When upvoting a question in a session" $ do
        describe "Authorization" $ do
            it "can be done by a Questioner in the session" fulfilledByTypes
            it "cannot be done by Answerers" fulfilledByTypes
        it "session must exist" pending
        it "question must exist" pending
        it "upvotes the question in the session" pending
        it "does not modify any other questions in the session" pending
        it "does not modify any other session" pending

answeringAQuestion = do
    describe "When answering a question in a session" $ do
        describe "Authorization" $ do
            it "can be done by the Answerer for the session" fulfilledByTypes
            it "cannot be done by Questioners" fulfilledByTypes
        it "session must exist" pending
        it "question must exist" pending
        it "does not modify any other questions in the session" pending
        it "does not modify any other session" pending
        it "answers the question in the session" pending

gettingFullSession = do
    describe "Getting the FullSession information" $ do
        describe "Authorization" $ do
            it "can be done by the Answerer for the session" fulfilledByTypes
            it "can be done by a Questioner in the session" fulfilledByTypes
        it "session must exist" pending
        it "does not modify anything" pending -- TODO: investigate making fulfilledByTypes
        it "returns the information for the specified session" pending
