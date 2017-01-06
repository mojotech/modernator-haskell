{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators, DataKinds, FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
module Main where

import Servant.Swagger.Test
import Test.Hspec
import Modernator.API
import Modernator.WebsocketsAPI
import Test.QuickCheck.Instances()
import Modernator.RequestBodies()
import Modernator.Types()
import Servant.Aeson.GenericSpecs (apiRoundtripSpecs, apiGoldenSpecs)
import Servant.API
import Servant.Aeson.Internal (HasGenericSpecs, MkTypeSpecs, collectRoundtripSpecs, mkTypeSpecs)
import GHC.TypeLits
import Data.Proxy
import Modernator.QuickCheck()

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
