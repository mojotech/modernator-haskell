{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Modernator.App where

import Modernator.Types
import Modernator.Commands
import Modernator.API
import Network.Wai (Application)
import Data.Acid
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Crypto.Random (drgNew)
import Data.Default (def)
import Modernator.Cookies
import Network.Wai (Request)
import Control.Concurrent.STM.TVar (newTVarIO, TVar)
import qualified Data.IxSet as IxSet
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)

-- | Serve our app
app :: RandomSource -> ServerKey -> ServerKey -> AcidState App -> TVar SessionChannelDB -> Application
app rng aKey qKey state sessionChannelDB =
    serveWithContext
        api
        ((defaultAuthHandler answererSettings aKey :: AuthHandler Request AnswererCookie) :.
         (defaultAuthHandler questionerSettings qKey :: AuthHandler Request QuestionerCookie) :.
         (anyAuthHandler (answererSettings, aKey) (questionerSettings, qKey) :: AuthHandler Request AnyCookie) :. EmptyContext)
        (server rng aKey qKey answererSettings questionerSettings state sessionChannelDB)
    where
        -- cookies are valid for 1 week
        -- TODO: I think it's theoretically possible to make these cookies only
        -- valid for specific session ids, since the session id is in the URL.
        -- This will easily allow users to be authed to multiple sessions.
        -- TODO: Should also have SecureOnly flag, but without https on localhost cookies won't be set
        answererSettings = def { acsCookieFlags = ["HttpOnly"], acsSessionField = "modernator_answerer", acsMaxAge = fromIntegral (3600 * 24 * 7 :: Integer) }
        questionerSettings = def { acsCookieFlags = ["HttpOnly"], acsSessionField = "modernator_questioner", acsMaxAge = fromIntegral (3600 * 24 * 7 :: Integer) }

-- | Set up our application potentially with a path to the application state.
mkApp :: Maybe FilePath -> IO Application
mkApp Nothing = openLocalState emptyApp >>= commonAppSetup
mkApp (Just path) = openLocalStateFrom path emptyApp >>= commonAppSetup

commonAppSetup state = do
    rng <- mkRandomSource drgNew 1000
    aKey <- mkServerKey 16 Nothing
    qKey <- mkServerKey 16 Nothing

    -- Initialize session channels for existing sessions
    sessionIds <- fmap (map (\ (Session id _ _ _) -> id) . IxSet.toList . sessions) $ query state GetState
    channels <- mapM mkSessionChannel sessionIds
    sessionChannelDB <- newTVarIO $ IxSet.fromList channels

    return $ app rng aKey qKey state sessionChannelDB

anyAuthHandler :: (AuthCookieSettings, ServerKey) -> (AuthCookieSettings, ServerKey) -> AuthHandler Request AnyCookie
anyAuthHandler (aSettings, aKey) (qSettings, qKey) = mkAuthHandler $ \ request -> do
    (asession :: Maybe (Either AnswererCookie QuestionerCookie)) <- liftIO (fmap (fmap Left) $ getSession aSettings aKey request)
    (qsession :: Maybe (Either AnswererCookie QuestionerCookie)) <- liftIO (fmap (fmap Right) $ getSession qSettings qKey request)
    maybe (throwError err403) return (asession <|> qsession)
