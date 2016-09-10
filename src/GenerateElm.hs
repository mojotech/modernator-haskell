module Main where

import Modernator.SessionsAPI
import Servant.Elm
import System.Environment (getArgs)
import Modernator.ElmTypes

spec = Spec ["Generated", "ModernatorAPI"] (defElmImports : generateElmForAPI (Proxy :: Proxy SessionsAPI))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Must enter a directory for output!"
        (x:_) -> specsToDir [spec] x
