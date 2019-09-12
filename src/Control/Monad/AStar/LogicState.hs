module Control.Monad.AStar.LogicState where

import Control.Monad.LogicState
import Data.Bifunctor
import Control.Monad.State

x :: LogicStateT () Int IO Int
x = do
    l <- backtrack (modify (second (+1)) >> get >>= liftIO . print >> return 1)
    r <- backtrack (get >>= liftIO . print >> return 2)
    l `interleave` r

run :: LogicStateT () Int IO a -> IO ([a], ((), Int))
run = observeStateManyT ((), 0) 10
