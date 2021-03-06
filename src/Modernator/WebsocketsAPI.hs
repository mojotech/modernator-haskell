{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, TypeFamilies, OverloadedStrings, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}
module Modernator.WebsocketsAPI where

import Modernator.Types
import Modernator.APIUtils
import Modernator.SessionsAPI (fullSessionHandler)

import Servant
import Data.Acid
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, readTChan)
import Control.Concurrent.STM.TVar (TVar)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions, acceptRequest, sendTextData, PendingConnection, sendClose, forkPingThread)
import Network.Wai (Application)
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as Text
import Servant.API.Verbs()
import Data.Aeson (encode)
import Servant.Swagger
import Data.Swagger.Lens
import Data.Swagger
import Control.Lens
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
    Capture "session_id" SessionId :> "messages" :> Websocket SessionMessage

websocketsAPI :: Proxy WebsocketsAPI
websocketsAPI = Proxy

websocketsServer ::
    AcidState App ->
    TVar SessionChannelDB ->
    Server WebsocketsAPI
websocketsServer app sessionChannelDB sessionId =
    websocketsOr
        defaultConnectionOptions
        (websocketsApp app sessionChannelDB sessionId)
        (backupApp app sessionId)

websocketsApp :: AcidState App -> TVar SessionChannelDB -> SessionId -> PendingConnection -> IO ()
websocketsApp app sessionChannelDB sessionId pending = do
    conn <- acceptRequest pending
    fullSessionE <- fullSessionHandler app sessionId
    let close m = sendClose conn (encode m)
    case fullSessionE of
        Left err -> close $ SessionExceptionMessage err
        Right fullSession ->
            withSessionChannel sessionChannelDB sessionId -- FIXME: this is searching for a session twice, once in fullSessionHandler and once here
                (close $ SessionExceptionMessage SessionNotFound)
                (\ channel -> do
                    duped <- atomically $ dupTChan channel
                    sendStartingMessage fullSession conn
                    -- keep the connection alive so wai doesn't kill it, TODO: this doesn't seem to actually require a pong
                    forkPingThread conn 30
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

backupApp app sessionId = serve backupAPI handler
    where
        handler = do
            resp <- liftIO $ fullSessionHandler app sessionId
            withError (fmap SessionState resp)
