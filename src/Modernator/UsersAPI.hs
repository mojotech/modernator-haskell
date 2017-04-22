{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies #-}
module Modernator.UsersAPI where

import Modernator.Types
import Modernator.Commands
import Modernator.APIUtils
import Modernator.RequestBodies
import Modernator.Cookies
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.Acid
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))

type instance AuthServerData (AuthProtect "user-auth") = ModernatorCookie

type UsersAPI =
    ReqBody '[JSON] LoginReq :> "login" :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] User)
    :<|> ReqBody '[JSON] UserReq :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] User)
    :<|> "me" :> AuthProtect "user-auth" :> Get '[JSON] User

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

usersServer ::
    (ModernatorCookie -> User -> Handler (Headers '[Header "Set-Cookie" EncryptedSession] User)) ->
    AcidState App ->
    Server UsersAPI
usersServer addUserSession app = loginH :<|> newUserH :<|> meH
    where
        newUserH req = do
            user@(User id _ _ _) <- liftIO . newUserHandler app $ req
            addUserSession (ModernatorCookie id) user
        meH = withError <=< liftIO . meHandler app
        loginH req = do
            user@(User id _ _ _) <- withError <=< liftIO . loginUserHandler app $ req
            addUserSession (ModernatorCookie id) user


-- TODO: Password rules?
newUserHandler :: AcidState App -> UserReq -> IO User
newUserHandler app (UserReq name password) = update app (NewUser name password)

meHandler :: AcidState App -> ModernatorCookie -> IO (Either AppError User)
meHandler app (ModernatorCookie id) = query app (GetUser id)

loginUserHandler :: AcidState App -> LoginReq -> IO (Either AppError User)
loginUserHandler app (LoginReq u p) = query app (AuthenticateUser u (hashPassword p))
