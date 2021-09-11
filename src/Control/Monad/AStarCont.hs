{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.AStarCont where

import Control.Monad.AStar.Class
import Control.Monad.Cont
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.IntPSQ as PSQ
import Control.Monad.Trans.Cont

tick :: MonadState Int m => m Int
tick = do
    n <- get
    modify succ
    pure n

type Q c v = IntPSQ c v

newtype AStarT r c m a = 
    AStarT {runAStarT :: (ContT r (StateT (Q c (AStarT r c m r)) (ReaderT c (StateT Int m))) a)}
    deriving newtype (Functor, Applicative, Monad)

instance Alternative (AStarT r c m) where
  empty = _
  (<|>) = _

instance (Monoid c, Ord c, Monad m) => MonadAStar c r (AStarT r c m) where
  spend c = AStarT $ shiftT $ \cc -> do
      runAStarT $ next c (AStarT . lift $ cc ())
  estimate = _
  done = _

stash :: (Semigroup c, Ord c, Monad m) => c -> AStarT r c m r -> AStarT r c m ()
stash c cc = AStarT $ do
  n <- lift . lift $ tick
  p <- asks (<> c)
  modify (PSQ.insert n p cc)

next :: (Monad m, Semigroup c, Ord c) => c -> AStarT r c m r -> AStarT r c m r
next c cc = do
    stash c cc
    q <- AStarT $ get
    let (result, newQ) = flip alterMin q $ \case
                            Just (_, _, nextAction) -> (nextAction, Nothing)
                            Nothing -> (cc, Nothing)
    AStarT $ put newQ
    result
