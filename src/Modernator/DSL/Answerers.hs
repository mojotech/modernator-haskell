{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Modernator.DSL.Answerers
(
  AnswerersDSL,
  createAnswerer,
  deleteAnswerer,
  pureI,
  ioI
)
where

import Control.Applicative ((<*))
import Control.Monad ((=<<))
import Control.Monad.Free
import Control.Monad.State
import Data.Text
import Modernator.DSL.Types
import Modernator.Types hiding (Answerer(..))
import qualified Data.IxSet as Ix

data Answerer = Answerer
    { answererSession :: Session
    , answererName :: Text
    }
    deriving (Show, Generic, Eq, Ord)

--TODO Session locked status as type var
class ImplAnswerers f where
    createAnswerer :: Text -> Session -> f Answerer
    deleteAnswerer :: Answerer -> f ()

data Answerers a
    = CreateAnswerer Session name (Answerer -> a)
    | DeleteAnswerer Answerer a
    deriving (Functor)

instance ImplAnswerers Answerers where
    createAnswerer s t = CreateAnswerer s t id
    deleteAnswerer a = DeleteAnswerer a ()

instance (ImplAnswerers f, Functor f) => ImplAnswerers (Free f) where
    createAnswerer s t = liftF $ createAnswerer s t
    deleteAnswerer a = liftF $ deleteAnswerer a

pureI :: PureInterpreter Answerers r
pureI (Free (CreateAnswerer session name cont)) = pureI . cont $ Answerer session name
pureI (Free (DeleteAnswerer a cont)) = pureI cont
pureI (Pure r) = pure r

ioI :: Interpreter IO Answerers r
ioI (Free (CreateAnswerer s t cont)) = putStrLn (show t) >> ioI (cont Answerer)
ioI (Free (DeleteAnswerer a cont)) = putStrLn "deleted" >> ioI (cont)
ioI (Pure a) = putStrLn "finished?" >> return a
