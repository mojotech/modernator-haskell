{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, TypeOperators, KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Modernator.MockApp where

{-
 - So, the mock server. It appears to still do request validation (bodies,
 - methods, content-types, etc.), which is good. It generates structurally
 - correct albeit arbitrary values, which is good.
 -
 - There were some hacks required to get this working though. First I needed to
 - implement an instance of HasMock for AuthProtect. The instance required
 - UndecidableInstances, which is a bad code smell. Second I had to disable
 - authentication on the mock server since it uses encrypted cookies, and
 - there's no way to get one. See
 - https://github.com/haskell-servant/servant/issues/596 for ongoing details
 - and solutions to those two problems.
 -}
import Modernator.SessionsAPI
import Network.Wai (Application)
import Modernator.App (AppContext)
import Servant
import Servant.Server.Experimental.Auth
import Modernator.Cookies
import Network.Wai (Request)
import Control.Monad.IO.Class (liftIO)
import Servant.Mock
import Test.QuickCheck.Instances() -- for ByteString instance
import Modernator.QuickCheck()
import GHC.TypeLits
import Test.QuickCheck

mockAPI :: Proxy ("sessions" :> SessionsAPI)
mockAPI = Proxy

-- | Mock application
app :: Application
app = serveWithContext mockAPI mockContext $ mock mockAPI (Proxy :: Proxy AppContext)

-- Always authenticates as an arbitrarily generated cookie
mockAuthHandler :: (Arbitrary a) => AuthHandler Request a
mockAuthHandler = mkAuthHandler $ \ _ -> liftIO $ generate arbitrary

mockContext :: Context AppContext
mockContext =
    ((mockAuthHandler :: AuthHandler Request ModernatorCookie) :. EmptyContext)

-- Mocking servers doesn't play well with auth
-- this needs undecidable instances but it seems to work okay?
instance ( HasMock rest context
         , HasServer rest context
         , HasContextEntry context (AuthHandler Request (AuthServerData (AuthProtect sym)))
         ) =>
         HasMock (AuthProtect (sym :: Symbol) :> rest) context where
    mock _ context = \_ -> mock (Proxy :: Proxy rest) context
