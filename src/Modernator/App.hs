{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds #-}
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
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

-- | Serve our app
app :: RandomSource -> ServerKey -> AcidState App -> TVar SessionChannelDB -> Application
app rng uKey state sessionChannelDB =
    serveWithContext api (appContext uKey) (server rng uKey userSettings state sessionChannelDB)

-- | Set up our application potentially with a path to the application state.
mkApp :: Maybe FilePath -> FilePath -> IO Application
mkApp Nothing keyDir = openLocalState emptyApp >>= commonAppSetup keyDir
mkApp (Just path) keyDir = openLocalStateFrom path emptyApp >>= commonAppSetup keyDir

boolToMaybe True = Just ()
boolToMaybe False = Nothing

getKeys :: FilePath -> IO ServerKey
getKeys keyDir = do
    userKeyM <- getKeyFromFile userPath
    uKey <- fromMaybe <$> mkServerKey 16 Nothing <*> pure userKeyM
    writeKey uKey userPath
    return (uKey)
    where
        userPath = keyDir ++ "user.key"
        writeKey key path = getServerKey key >>= \k -> BS.writeFile path k

getKeyFromFile path = do
    pathM <- fmap (fmap (const path) . boolToMaybe) <$> doesFileExist $ path
    case pathM of
        Just p -> Just <$> (BS.readFile p >>= \bs -> mkServerKeyFromBytes bs)
        Nothing -> return Nothing

commonAppSetup keyDir state = do
    rng <- mkRandomSource drgNew 1000
    (uKey) <- getKeys keyDir

    -- Initialize session channels for existing sessions
    sessionIds <- fmap (map (\ (Session id _ _ _) -> id) . IxSet.toList . sessions) $ query state GetState
    channels <- mapM mkSessionChannel sessionIds
    sessionChannelDB <- newTVarIO $ IxSet.fromList channels

    return $ app rng uKey state sessionChannelDB

type AppContext =
    '[ AuthHandler Request ModernatorCookie
     ]

appContext :: ServerKey -> Context AppContext
appContext uKey =
    ((defaultAuthHandler userSettings uKey :: AuthHandler Request ModernatorCookie) :. EmptyContext)

-- cookies are valid for 1 week
-- TODO: I think it's theoretically possible to make these cookies only
-- valid for specific session ids, since the session id is in the URL.
-- This will easily allow users to be authed to multiple sessions.
-- TODO: Should also have SecureOnly flag, but without https on localhost cookies won't be set
userSettings = def { acsCookieFlags = ["HttpOnly"], acsSessionField = "modernator", acsMaxAge = fromIntegral (3600 * 24 * 365 :: Integer) }
