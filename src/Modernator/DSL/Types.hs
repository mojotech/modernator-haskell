module Modernator.DSL.Types where

import Control.Monad.Free
import Control.Monad.Identity

type Interpreter m f r = Free f r -> m r
type PureInterpreter f r = Interpreter Identity f r
