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

main = hspec spec

instance Arbitrary SessionReq where
    arbitrary = SessionReq <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionReq where
    arbitrary = QuestionReq <$> arbitrary

instance Arbitrary JoinReq where
    arbitrary = JoinReq <$> arbitrary

instance Arbitrary Questioner where
    arbitrary = Questioner <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionerId where
    arbitrary = QuestionerId <$> arbitrary

instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

instance Arbitrary Question where
    arbitrary = Question <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionId where
    arbitrary = QuestionId <$> arbitrary

instance Arbitrary Votes where
    arbitrary = Votes <$> arbitrary

instance Arbitrary Answered where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Answerer where
    arbitrary = Answerer <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AnswererId where
    arbitrary = AnswererId <$> arbitrary

constArb a = fmap (const a) (arbitrary :: Gen ())

-- template haskell this
instance Arbitrary SessionMessage where
    arbitrary = oneof
        [ constArb SessionLocked
        , constArb SessionClosed
        , constArb SessionExpired
        , fmap SessionExceptionMessage arbitrary
        , fmap SessionState arbitrary
        ]

instance Arbitrary AppError where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary FullSession where
    arbitrary = FullSession <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Session where
    arbitrary = Session <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LockedStatus where
    arbitrary = arbitraryBoundedEnum
