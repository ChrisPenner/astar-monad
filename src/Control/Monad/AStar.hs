{-|
Module      : Control.Monad.AStar
Description : Provides a monad transformer for A* search. See README for details and examples.
Copyright   : (c) Chris Penner, 2019
License     : BSD3

See the <https://github.com/chrispenner/astar-monad README> for usage info and examples.

-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.AStar
    (
    -- * Types
      AStar
    , AStarT

    -- * Methods
    , MonadAStar(..)

    -- * Executing Search
    , runAStarT
    , execAStarT
    , evalAStarT
    , runAStar
    , execAStar
    , evalAStar

    , tryWhile
    , tryWhileT
    )
    where

import Control.Monad.AStar.Class
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe

data Step c r a = Pure a | Weighted c | Solved r
    deriving (Show, Functor, Eq)

-- | Non-transformer version of 'AStarT'
type AStar s c r a = AStarT s c r Identity a

-- | The 'AStar' search monad transformer
--
-- Lots of type variables here:
--
-- @s@: State; keep anything you want in here, it will stay coherent across
-- branch switches.
--
-- @c@: Cost measure: The type you'll use for determining the estimated cost of following a given
-- branch. Usually requires 'Ord'.
--
-- @r@: Result type, this is often redundant to State but is provided for convenience.
-- This is the type you pass to 'done' when you've found a solution.
--
-- @m@: An arbitrary monad which will be threaded through.
--
-- Be wary that effects will be run in seemingly non-deterministic ordering as we switch
-- chaotically between branches.
newtype AStarT s c r m a =
    AStarT { unAStarT :: StateT s (LogicT m) (Step c r a)
          } deriving stock Functor

-- mapResult :: (r -> r') -> AStarT s c r m a -> AStarT s c r' m a
-- mapResult f (AStarT m) = AStarT $ fmap go m
--   where
--     go (Pure a) = Pure a
--     go (Weighted c) = Weighted c
--     go (Solved r) = Solved $ f r

instance MonadTrans (AStarT s c r) where
  lift m = AStarT . lift . lift $ (Pure <$> m)

instance (MonadIO m, Ord c) => MonadIO (AStarT s c r m) where
  liftIO io = lift $ liftIO io

instance (Monad m, Ord c) => Applicative (AStarT s c r m) where
  pure = return
  (<*>) = ap

instance (Ord c, Monad m) => MonadPlus (AStarT s c r m) where
  mzero = empty
  mplus = (<|>)

instance (Ord c, Monad m) => MonadFail (AStarT s c r m) where
  fail _ = empty

instance (Ord c, Monad m) => MonadState s (AStarT s c r m) where
  get = AStarT $ Pure <$> get
  put s = AStarT $ Pure <$> put s

instance (Monad m, Ord c) => Monad (AStarT s c r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      msplit m >>= \case
        Nothing -> empty
        Just (Pure a, continue) -> unAStarT $ (f a) `weightedInterleave` (AStarT continue >>= f)
        Just (Solved r, _) -> pure $ Solved r
        Just (Weighted c, continue) -> do
            reflect $ Just (Weighted c, unAStarT $ AStarT continue >>= f)

instance (Ord c, Monad m) => Alternative (AStarT s c r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

weightedInterleave :: (Ord c, Monad m) => AStarT s c r m a -> AStarT s c r m a -> AStarT s c r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord c, MonadLogic m, MonadState s m) => m (Step c r a) -> m (Step c r a) -> m (Step c r a)
weightedInterleave' ma mb = do
    beforeBoth <- get
    (rA, lState) <- liftA2 (,) (msplit ma) get
    put beforeBoth
    (rB, rState) <- liftA2 (,) (msplit mb) get
    case (rA, rB) of
        (m, Nothing) -> put lState >> reflect m
        (Nothing, m) -> put rState >> reflect m
        (Just (Solved a, _), _) -> put lState >> pure (Solved a)
        (_ , Just (Solved a, _)) -> put rState >> pure (Solved a)
        (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm)))
          | lw < rw ->
              (put lState >> pure (Weighted lw))
                <|> ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
          | otherwise ->
              (put rState >> pure (Weighted rw))
               <|> ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))
        (l, r) -> (put lState >> reflect l) `weightedInterleave'` (put rState >> reflect r)

-- | Run an A* computation effect returning the solution and branch state if one was found.
runAStarT :: (Monad m) => AStarT s c r m a -> s -> m (Maybe (r, s))
runAStarT (AStarT m) s = fmap listToMaybe . observeManyT 1 $ do
    runStateT m s >>= \case
      (Solved a, s) -> return (a, s)
      _ -> empty

-- | Run a pure A* computation returning the solution and branch state if one was found.
runAStar :: AStar s c r a -> s -> Maybe (r, s)
runAStar m s = runIdentity $ runAStarT  m s

-- | Run an effectful A* computation returning only the branch state
execAStarT :: (Monad m) => AStarT s c r m a -> s -> m (Maybe s)
execAStarT m s = fmap snd <$> runAStarT m s

-- | Run an effectful A* computation returning only the solution
evalAStarT :: (Monad m) => AStarT s c r m a -> s -> m (Maybe r)
evalAStarT m s = fmap fst <$> runAStarT m s

-- | Run a pure A* computation returning only the branch state
execAStar :: AStar s c r a -> s -> (Maybe s)
execAStar m s = fmap snd $ runAStar m s

-- | Run a pure A* computation returning only the solution
evalAStar :: AStar s c r a -> s -> (Maybe r)
evalAStar m s = fmap fst $ runAStar m s

-- | Run a pure A* search but short-circuit when the lowest cost fails a predicate.
--
-- This is useful for detecting if your search is diverging, or is likely to fail.
tryWhile :: (c -> Bool) -> AStar s c r a -> s -> (Maybe (r, s))
tryWhile p m s = runIdentity $ tryWhileT p m s

-- | Effectful version of 'tryWhile'
tryWhileT :: Monad m => (c -> Bool) -> AStarT s c r m a -> s -> m (Maybe (r, s))
tryWhileT p m s = do
    stepAStar m s >>= \case
      Nothing -> return Nothing
      Just ((Pure _, s), continue) -> tryWhileT p continue s
      Just ((Weighted c, s), continue) ->
          if p c then tryWhileT p continue s
                 else return Nothing
      Just ((Solved r, s), _) -> return (Just (r, s))

stepAStar :: (Monad m) => AStarT s c r m a -> s -> m (Maybe ((Step c r a, s), AStarT s c r m a))
stepAStar (AStarT m) s = fmap (fmap go) . observeT . (fmap . fmap . fmap . fmap) fst $ msplit (runStateT m s)
  where
    go (v, x) = (v, AStarT (lift x))

instance (Ord w, Monad m) => MonadAStar w r (AStarT s w r m) where
  branch = (<|>)
  updateCost c = AStarT $ pure (Weighted c) <|> return (Pure ())
  done = AStarT . pure . Solved
