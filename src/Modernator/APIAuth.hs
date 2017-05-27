{-# LANGUAGE DataKinds, TypeFamilies #-}
module Modernator.APIAuth where

import Modernator.Cookies
import Servant
import Servant.Server.Experimental.Auth

type instance AuthServerData (AuthProtect "user-auth") = ModernatorCookie
