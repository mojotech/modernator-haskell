{-# LANGUAGE OverloadedStrings #-}
module Main where

import Modernator.App
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intersperse)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsOrigins, corsRequestHeaders)
import qualified Data.ByteString.Char8 as BS

prettyArg s = fmap ((s ++) . show)

main = do
    port <- fmap (fromMaybe 8080 . (>>= readMaybe)) $ lookupEnv "MODERNATOR_PORT"
    stateDir <- lookupEnv "MODERNATOR_STATE_DIR"
    keyDir <- fmap (fromMaybe "./") $ lookupEnv "MODERNATOR_KEY_DIR"
    extraCors <- fmap (map BS.pack . words . fromMaybe "") $ lookupEnv "MODERNATOR_CORS"
    let args = mapMaybe id [ prettyArg "port: " (pure port)
                           , prettyArg "state directory: " stateDir
                           , prettyArg "key directory: " (pure keyDir)
                           , prettyArg "extra CORS: " (pure extraCors)
                           ]
    putStrLn $ "Running modernator-haskell on " ++ (concat $ intersperse " and " args)
    app <- mkApp stateDir keyDir
    run port $ logStdoutDev $ myCors extraCors app

myCors extra = cors (const $ Just (policy extra))

policy extra = simpleCorsResourcePolicy
    { corsOrigins = Just ([ "http://localhost:8080" ] ++ extra, True)
    , corsRequestHeaders = ["Content-Type"]
    }
