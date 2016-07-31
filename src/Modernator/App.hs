module Modernator.App where

import Modernator.Types
import Modernator.API
import Network.Wai (Application)
import Data.Acid
import Servant

-- | Serve our app
app :: AcidState App -> Application
app = serve api . server

-- | Set up our application potentially with a path to the application state.
mkApp :: Maybe FilePath -> IO Application
mkApp Nothing = fmap app $ openLocalState emptyApp
mkApp (Just path) = fmap app $ openLocalStateFrom path emptyApp
