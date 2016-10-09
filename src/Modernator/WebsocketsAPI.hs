{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies, OverloadedStrings, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}
module Modernator.WebsocketsAPI where

import Modernator.Types
import Modernator.Commands
import Modernator.APIUtils
import Modernator.Cookies
import Modernator.SessionsAPI (fullSessionHandler, appToServant)

import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.Proxy
import Data.Acid
import Data.Foldable (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<), forever)
import Control.Monad.STM (atomically, STM)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.STM.TChan (newBroadcastTChan, writeTChan, dupTChan, readTChan, tryReadTChan, peekTChan, TChan, unGetTChan, cloneTChan)
import Control.Concurrent.STM.TVar (readTVar, TVar, modifyTVar')
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions, acceptRequest, sendBinaryData, sendTextData, PendingConnection, sendClose)
import Network.Wai (Application)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant.API.Verbs
import Data.Aeson (encode)
import qualified Data.IxSet as Ix
import Servant.Swagger
import Data.Swagger.Lens
import Data.Swagger
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict.InsOrd (adjust)

data Websocket a = Websocket

instance HasServer (Websocket a) c where
    type ServerT (Websocket a) m = Application

    route _ = route (Proxy :: Proxy Raw)

-- Useful for adjusting the swagger definitions for an operation on an endpoint path
adjustPathItemOperation op f path = paths %~ adjust (\ p -> p & op %~ (fmap f)) path

instance HasSwagger (Websocket a) where
    toSwagger _ = toSwagger (Proxy :: Proxy BackupAPI)
        & adjustPathItemOperation get (description ?~ ("This endpoint representation may additionally be implemented as a websocket endpoint. There's no way to represent websocket endpoints in Swagger at the moment so this representation pulls double duty. The HTTP endpoint must only return `SessionState` messages. The websocket endpoint can send any of the enumerated message types, but it must start transmission with a `SessionState` message." :: Text)) "/"
        & adjustPathItemOperation get (schemes .~ Just [Ws, Http]) "/"

type WebsocketsAPI =
    AuthProtect "any-auth" :> Capture "session_id" SessionId :> "messages" :> Websocket SessionMessage

websocketsAPI :: Proxy WebsocketsAPI
websocketsAPI = Proxy

websocketsServer ::
    AcidState App ->
    TVar SessionChannelDB ->
    Server WebsocketsAPI
websocketsServer app sessionChannelDB cookie sessionId =
    websocketsOr
        defaultConnectionOptions
        (websocketsApp app sessionChannelDB cookie sessionId)
        (backupApp app cookie sessionId)

websocketsApp :: AcidState App -> TVar SessionChannelDB -> AnyCookie -> SessionId -> PendingConnection -> IO ()
websocketsApp app sessionChannelDB cookie sessionId pending = do
    conn <- acceptRequest pending
    fullSessionE <- runExceptT $ fullSessionHandler app cookie sessionId
    let close m = sendClose conn (encode m)
    case fullSessionE of
        Left err -> close $ SessionExceptionMessage err
        Right fullSession ->
            withSessionChannel sessionChannelDB sessionId -- FIXME: this is searching for a session twice, once in fullSessionHandler and once here
                (close $ SessionExceptionMessage SessionNotFound)
                (\ channel -> do
                    duped <- atomically $ dupTChan channel
                    sendStartingMessage fullSession conn
                    sendMessages duped conn)

sendJsonData conn x = sendTextData conn (Text.decodeUtf8 (encode x))

sendStartingMessage fullSession conn = sendJsonData conn (SessionState fullSession)
sendMessages channel conn = forever $ atomically (readTChan channel) >>= sendJsonData conn

-- You might expect this to be "sessions" :> Capture "session_id" :> "socket" :> Get '[JSON] SessionMessage
-- It's not because it receives a request with all of the routing information
-- (pathInfo field) stripped out of it, as the request has already undergone
-- routing.
type BackupAPI = Get '[JSON] SessionMessage

backupAPI :: Proxy BackupAPI
backupAPI = Proxy

backupApp app cookie sessionId = serve backupAPI handler
    where
        handler = fmap SessionState $ appToServant $ fullSessionHandler app cookie sessionId
