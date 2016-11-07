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

prettyArg s = fmap ((s ++) . show)

main = do
    port <- fmap (fromMaybe 8080 . (>>= readMaybe)) $ lookupEnv "MODERNATOR_PORT"
    stateDir <- lookupEnv "MODERNATOR_STATE_DIR"
    let args = mapMaybe id [ prettyArg "port: " (pure port)
                           , prettyArg "state directory: " stateDir
                           ]
    putStrLn $ "Running modernator-haskell on " ++ (concat $ intersperse " and " args)
    app <- mkApp stateDir
    run port $ logStdoutDev $ myCors app

myCors = cors (const $ Just policy)

policy = simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3000", "http://localhost:8080"], True)
    , corsRequestHeaders = ["Content-Type"]
    }
