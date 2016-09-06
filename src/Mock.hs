module Main where

import Modernator.MockApp
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intersperse)
import Network.Wai.Middleware.RequestLogger

prettyArg s = fmap ((s ++) . show)

main = do
    port <- fmap (fromMaybe 8080 . (>>= readMaybe)) $ lookupEnv "MODERNATOR_PORT"
    let args = mapMaybe id [ prettyArg "port: " (pure port)
                           ]
    putStrLn $ "Running modernator-haskell on " ++ (concat $ intersperse " and " args)
    run port $ logStdoutDev app
