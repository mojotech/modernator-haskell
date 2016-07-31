module Main where

import Modernator.App
import Network.Wai.Handler.Warp

main = mkApp Nothing >>= run 8080
