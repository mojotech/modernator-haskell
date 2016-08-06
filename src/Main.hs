module Main where

import Modernator.App
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intersperse)
import Network.Wai.Middleware.RequestLogger

prettyArg s = fmap ((s ++) . show)

main = do
    port <- fmap (fromMaybe 8080 . (>>= readMaybe)) $ lookupEnv "MODERNATOR_PORT"
    stateDir <- lookupEnv "MODERNATOR_STATE_DIR"
    let args = mapMaybe id [ prettyArg "port: " (pure port)
                           , prettyArg "state directory: " stateDir
                           ]
    putStrLn $ "Running modernator-haskell on " ++ (concat $ intersperse " and " args)
    app <- mkApp stateDir
    run port $ logStdoutDev app
