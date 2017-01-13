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
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

-- | Serve our app
app :: RandomSource -> ServerKey -> ServerKey -> AcidState App -> TVar SessionChannelDB -> Application
app rng aKey qKey state sessionChannelDB =
    serveWithContext api (appContext aKey qKey) (server rng aKey qKey answererSettings questionerSettings state sessionChannelDB)

-- | Set up our application potentially with a path to the application state.
mkApp :: Maybe FilePath -> FilePath -> IO Application
mkApp Nothing keyDir = openLocalState emptyApp >>= commonAppSetup keyDir
mkApp (Just path) keyDir = openLocalStateFrom path emptyApp >>= commonAppSetup keyDir

boolToMaybe True = Just ()
boolToMaybe False = Nothing

getKeys :: FilePath -> IO (ServerKey, ServerKey)
getKeys keyDir = do
    questionerKeyM <- getKeyFromFile questionerPath
    answererKeyM <- getKeyFromFile answererPath
    qKey <- fromMaybe <$> mkServerKey 16 Nothing <*> pure questionerKeyM
    aKey <- fromMaybe <$> mkServerKey 16 Nothing <*> pure answererKeyM
    writeKey qKey questionerPath
    writeKey aKey answererPath
    return (aKey, qKey)
    where
        questionerPath = keyDir ++ "questioner.key"
        answererPath = keyDir ++ "answerer.key"
        writeKey key path = getServerKey key >>= \k -> BS.writeFile path k

getKeyFromFile path = do
    pathM <- fmap (fmap (const path) . boolToMaybe) <$> doesFileExist $ path
    case pathM of
        Just p -> Just <$> (BS.readFile p >>= \bs -> mkServerKeyFromBytes bs)
        Nothing -> return Nothing

commonAppSetup keyDir state = do
    rng <- mkRandomSource drgNew 1000
    (aKey, qKey) <- getKeys keyDir

    -- Initialize session channels for existing sessions
    sessionIds <- fmap (map (\ (Session id _ _ _) -> id) . IxSet.toList . sessions) $ query state GetState
    channels <- mapM mkSessionChannel sessionIds
    sessionChannelDB <- newTVarIO $ IxSet.fromList channels

    return $ app rng aKey qKey state sessionChannelDB

type AppContext =
    '[ AuthHandler Request AnswererCookie
     , AuthHandler Request QuestionerCookie
     , AuthHandler Request AnyCookie
     ]

appContext :: ServerKey -> ServerKey -> Context AppContext
appContext aKey qKey =
    ((defaultAuthHandler answererSettings aKey :: AuthHandler Request AnswererCookie) :.
     (defaultAuthHandler questionerSettings qKey :: AuthHandler Request QuestionerCookie) :.
     (anyAuthHandler (answererSettings, aKey) (questionerSettings, qKey) :: AuthHandler Request AnyCookie) :. EmptyContext)

-- cookies are valid for 1 week
-- TODO: I think it's theoretically possible to make these cookies only
-- valid for specific session ids, since the session id is in the URL.
-- This will easily allow users to be authed to multiple sessions.
-- TODO: Should also have SecureOnly flag, but without https on localhost cookies won't be set
answererSettings = def { acsCookieFlags = ["HttpOnly"], acsSessionField = "modernator_answerer", acsMaxAge = fromIntegral (3600 * 24 * 7 :: Integer) }
questionerSettings = def { acsCookieFlags = ["HttpOnly"], acsSessionField = "modernator_questioner", acsMaxAge = fromIntegral (3600 * 24 * 7 :: Integer) }

anyAuthHandler :: (AuthCookieSettings, ServerKey) -> (AuthCookieSettings, ServerKey) -> AuthHandler Request AnyCookie
anyAuthHandler (aSettings, aKey) (qSettings, qKey) = mkAuthHandler $ \ request -> do
    (asession :: Maybe (Either AnswererCookie QuestionerCookie)) <- liftIO (fmap (fmap Left) $ getSession aSettings aKey request)
    (qsession :: Maybe (Either AnswererCookie QuestionerCookie)) <- liftIO (fmap (fmap Right) $ getSession qSettings qKey request)
    maybe (throwError err403) return (asession <|> qsession)
